{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sync (syncSpec) where

import Data.IntSet (IntSet)
import Effectful (Eff, runPureEff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Local (State)
import Effectful.Writer.Static.Local (Writer)
import GHC.Stack (HasCallStack)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Data.IntSet qualified as IntSet
import Effectful.State.Static.Local qualified as State
import Effectful.Writer.Static.Local qualified as Writer

import Configuration qualified as Config
import Git (BaseBranch (..), Branch (..), Sha (..))
import Git qualified
import Logic (Action (..), Event (..), RetrieveEnvironment (..))
import Logic qualified
import Project (Approval (..), BuildStatus (..), Check (..), IntegrationStatus (..), Priority (..), PullRequest)
import TestSetup (fakeRunTime, testProjectConfig, testTime, testTimeouts, testTriggerConfig)
import Time (TimeOperation)
import Types (PullRequestId (..), Username (..))

import GithubApi qualified
import Project qualified

data ActionFlat
  = AGetPullRequest PullRequestId
  | AGetOpenPullRequests
  | AGetBuildStatus Sha
  | ALeaveComment PullRequestId String
  | ATryPromote Sha
  | ATryForcePush Branch Sha
  | ACleanupTestBranch PullRequestId
  deriving (Eq, Show)

data GithubResults = GithubResults
  { resultGetPullRequest :: [Maybe GithubApi.PullRequest]
  , resultGetOpenPullRequests :: [Maybe IntSet]
  , resultGetBuildStatus :: [Maybe [(Check, BuildStatus)]]
  , resultPromote :: [Git.PushResult]
  }

defaultGithubResults :: GithubResults
defaultGithubResults =
  GithubResults
    { resultGetPullRequest = repeat Nothing
    , resultGetOpenPullRequests = repeat Nothing
    , resultGetBuildStatus = repeat Nothing
    , resultPromote = repeat Git.PushOk
    }

takeFromList
  :: HasCallStack
  => State GithubResults :> es
  => String
  -> (GithubResults -> [a])
  -> ([a] -> GithubResults -> GithubResults)
  -> Eff es a
takeFromList name getField setField = do
  values <- State.gets getField
  State.modify $ setField $ tail values
  case values of
    [] -> error $ "Not enough results supplied for " <> name <> "."
    v : _ -> pure v

takeResultGetPullRequest :: (HasCallStack, State GithubResults :> es) => Eff es (Maybe GithubApi.PullRequest)
takeResultGetPullRequest =
  takeFromList
    "resultGetPullRequest"
    resultGetPullRequest
    (\v res -> res{resultGetPullRequest = v})

takeResultGetOpenPullRequests :: (HasCallStack, State GithubResults :> es) => Eff es (Maybe IntSet)
takeResultGetOpenPullRequests =
  takeFromList
    "resultGetOpenPullRequests"
    resultGetOpenPullRequests
    (\v res -> res{resultGetOpenPullRequests = v})

takeResultGetBuildStatus :: (HasCallStack, State GithubResults :> es) => Eff es (Maybe [(Check, BuildStatus)])
takeResultGetBuildStatus =
  takeFromList
    "resultGetBuildStatus"
    resultGetBuildStatus
    (\v res -> res{resultGetBuildStatus = v})

runMockGithub
  :: State GithubResults :> es
  => Eff (GithubApi.GithubOperation : es) a
  -> Eff es a
runMockGithub =
  interpret $ \_ -> \case
    GithubApi.LeaveComment _ _ -> pure ()
    GithubApi.AddReaction _ _ -> pure ()
    GithubApi.HasPushAccess _ -> pure False
    GithubApi.GetPullRequest _ -> takeResultGetPullRequest
    GithubApi.GetOpenPullRequests -> takeResultGetOpenPullRequests
    GithubApi.GetBuildStatus _ -> takeResultGetBuildStatus

runSyncAction
  :: (GithubApi.GithubOperation :> es, Writer [ActionFlat] :> es)
  => Eff (Action : es) a
  -> Eff es a
runSyncAction =
  interpret $ \_ -> \case
    GetPullRequest pr -> do
      Writer.tell [AGetPullRequest pr]
      GithubApi.getPullRequest pr
    GetOpenPullRequests -> do
      Writer.tell [AGetOpenPullRequests]
      GithubApi.getOpenPullRequests
    GetBuildStatus sha -> do
      Writer.tell [AGetBuildStatus sha]
      GithubApi.getBuildStatus sha
    TryPromote sha -> do
      Writer.tell [ATryPromote sha]
      pure Git.PushOk
    TryForcePush branch sha -> do
      Writer.tell [ATryForcePush branch sha]
      pure Git.PushOk
    LeaveComment pr _ -> do
      Writer.tell [ALeaveComment pr "message"]
      pure ()
    CleanupTestBranch pr -> do
      Writer.tell [ACleanupTestBranch pr]
      pure ()
    -- Stub out other actions that might be called during proceedUntilFixedPoint
    IsReviewer _ -> pure False
    AddReaction _ _ -> pure ()
    TryIntegrate _ _ _ -> pure $ Left (Logic.IntegrationFailure (BaseBranch "master") Git.MergeFailed)
    TryPromoteWithTag _ _ _ -> pure (Left "error", Git.PushOk)
    GetLatestVersion _ -> pure $ Right 1
    GetChangelog _ _ -> pure Nothing
    IncreaseMergeAttemptedMetric _ -> pure ()
    IncreaseMergeFailedMetric _ _ -> pure ()
    IncreaseMergeMetric _ -> pure ()
    UpdateTrainSizeMetric _ -> pure ()

runRetrieveInfo
  :: State GithubResults :> es
  => Eff (RetrieveEnvironment : es) a
  -> Eff es a
runRetrieveInfo = interpret $ \_ -> \case
  Logic.GetProjectConfig -> pure testProjectConfig
  Logic.GetDateTime -> pure testTime
  Logic.GetBaseBranch -> pure (BaseBranch $ Config.branch testProjectConfig)

runSyncWithHandle
  :: GithubResults
  -> (forall es. (Action :> es, TimeOperation :> es, RetrieveEnvironment :> es, GithubApi.GithubOperation :> es, State GithubResults :> es, Writer [ActionFlat] :> es) => Eff es a)
  -> (a, [ActionFlat])
runSyncWithHandle results eff =
  runPureEff $ Writer.runWriter $ State.evalState results $ runMockGithub $ runRetrieveInfo $ fakeRunTime $ runSyncAction eff

mkExternalPullRequest :: PullRequestId -> GithubApi.PullRequest
mkExternalPullRequest _ =
  GithubApi.PullRequest
    { GithubApi.sha = Sha "7faa52318"
    , GithubApi.branch = Branch "nexus-7"
    , GithubApi.baseBranch = BaseBranch "master"
    , GithubApi.title = "Add Nexus 7 experiment"
    , GithubApi.author = Username "tyrell"
    }

insertInitialPr :: PullRequestId -> Project.ProjectState -> Project.ProjectState
insertInitialPr prId =
  Project.insertPullRequest
    prId
    (Branch "existing")
    (BaseBranch "master")
    (Sha "abc1234")
    "Existing"
    (Username "deckard")

lookupPr :: PullRequestId -> Project.ProjectState -> PullRequest
lookupPr prId state =
  case Project.lookupPullRequest prId state of
    Just pr -> pr
    Nothing -> error "Expected pull request to exist."

syncSpec :: Spec
syncSpec =
  describe "Logic.synchronizeState via handleEvent" $ do
    it "keeps existing PRs when they are still open on GitHub" $ do
      let
        prId = PullRequestId 1
        state0 = insertInitialPr prId Project.emptyProjectState
        results =
          defaultGithubResults
            { resultGetOpenPullRequests = [Just $ IntSet.singleton 1]
            , resultGetPullRequest =
                [ Just
                    GithubApi.PullRequest
                      { GithubApi.sha = Sha "abc1234"
                      , GithubApi.branch = Branch "existing"
                      , GithubApi.baseBranch = BaseBranch "master"
                      , GithubApi.title = "Existing"
                      , GithubApi.author = Username "deckard"
                      }
                ]
            }
        (state', actions) =
          runSyncWithHandle results $
            Logic.handleEvent testTriggerConfig (Config.MergeWindowExemptionConfiguration []) Nothing testTimeouts Synchronize state0

      state' `shouldBe` state0
      actions `shouldBe` [AGetOpenPullRequests, AGetPullRequest prId]

    it "synchronizes when no PRs exist locally" $ do
      let
        prId = PullRequestId 17
        results =
          defaultGithubResults
            { resultGetOpenPullRequests = [Just $ IntSet.singleton 17]
            , resultGetPullRequest = [Just $ mkExternalPullRequest prId]
            }
        (state', actions) =
          runSyncWithHandle results $
            Logic.handleEvent testTriggerConfig (Config.MergeWindowExemptionConfiguration []) Nothing testTimeouts Synchronize Project.emptyProjectState

      state' `shouldSatisfy` Project.existsPullRequest prId
      let pr = lookupPr prId state'
      Project.title pr `shouldBe` "Add Nexus 7 experiment"
      Project.author pr `shouldBe` Username "tyrell"
      Project.branch pr `shouldBe` Branch "nexus-7"
      Project.sha pr `shouldBe` Sha "7faa52318"
      Project.approval pr `shouldBe` Nothing
      Project.integrationStatus pr `shouldBe` Project.NotIntegrated
      Project.integrationAttempts pr `shouldBe` []
      actions `shouldBe` [AGetOpenPullRequests, AGetPullRequest prId]

    it "removes PRs that are no longer open on GitHub" $ do
      let
        prId = PullRequestId 1
        state0 = insertInitialPr prId Project.emptyProjectState
        results =
          defaultGithubResults
            { resultGetOpenPullRequests = [Just IntSet.empty]
            }
        (state, _actions) =
          runSyncWithHandle results $
            Logic.handleEvent testTriggerConfig (Config.MergeWindowExemptionConfiguration []) Nothing testTimeouts Synchronize state0

      state `shouldBe` Project.emptyProjectState

    it "does not modify the state when querying open pull requests fails" $ do
      let
        prId = PullRequestId 19
        state0 = insertInitialPr prId Project.emptyProjectState
        results =
          defaultGithubResults
            { resultGetOpenPullRequests = [Nothing]
            }
        (state', actions) =
          runSyncWithHandle results $
            Logic.handleEvent testTriggerConfig (Config.MergeWindowExemptionConfiguration []) Nothing testTimeouts Synchronize state0

      state' `shouldBe` state0
      actions `shouldBe` [AGetOpenPullRequests]

    it "queries details of existing pull requests during synchronize" $ do
      let
        prId = PullRequestId 19
        state0 = insertInitialPr prId Project.emptyProjectState
        results =
          defaultGithubResults
            { resultGetOpenPullRequests = [Just $ IntSet.singleton 19]
            , resultGetPullRequest =
                [ Just
                    GithubApi.PullRequest
                      { GithubApi.sha = Sha "abc1234"
                      , GithubApi.branch = Branch "existing"
                      , GithubApi.baseBranch = BaseBranch "master"
                      , GithubApi.title = "Existing"
                      , GithubApi.author = Username "deckard"
                      }
                ]
            }
        (_state, actions) =
          runSyncWithHandle results $
            Logic.handleEvent testTriggerConfig (Config.MergeWindowExemptionConfiguration []) Nothing testTimeouts Synchronize state0

      actions `shouldBe` [AGetOpenPullRequests, AGetPullRequest prId]

    it "force-pushes after successful build is detected via sync" $ do
      let
        prId = PullRequestId 1
        integratedSha = Sha "84c"
        approval = Approval (Username "deckard") Nothing Project.Merge 0 Nothing Normal
        state0 =
          Project.setApproval prId (Just approval) $
            Project.setIntegrationStatus prId (Integrated integratedSha (Project.AnyCheck BuildPending)) $
              insertInitialPr prId Project.emptyProjectState
        results =
          defaultGithubResults
            { resultGetOpenPullRequests = [Just $ IntSet.singleton 1]
            , resultGetBuildStatus = [Just [(Check "build", BuildSucceeded)]]
            , resultPromote = [Git.PushOk]
            }
        (_state, actions) =
          runSyncWithHandle results $
            Logic.handleEvent testTriggerConfig (Config.MergeWindowExemptionConfiguration []) Nothing testTimeouts Synchronize state0

      actions `shouldBe` [AGetOpenPullRequests, AGetPullRequest prId, AGetBuildStatus integratedSha, ATryForcePush (Branch "existing") integratedSha]

    it "promotes PR when sync reveals force-pushed commit on the PR branch" $ do
      let
        prId = PullRequestId 1
        integratedSha = Sha "84c"
        approval = Approval (Username "deckard") Nothing Project.Merge 0 Nothing Normal
        state0 =
          Project.setApproval prId (Just approval) $
            Project.setIntegrationStatus prId (Promote testTime integratedSha) $
              insertInitialPr prId Project.emptyProjectState
        results =
          defaultGithubResults
            { resultGetOpenPullRequests = [Just $ IntSet.singleton 1]
            , resultGetPullRequest =
                [ Just
                    GithubApi.PullRequest
                      { GithubApi.sha = integratedSha
                      , GithubApi.branch = Branch "existing"
                      , GithubApi.baseBranch = BaseBranch "master"
                      , GithubApi.title = "Existing"
                      , GithubApi.author = Username "deckard"
                      }
                ]
            }
        (_state, actions) =
          runSyncWithHandle results $
            Logic.handleEvent testTriggerConfig (Config.MergeWindowExemptionConfiguration []) Nothing testTimeouts Synchronize state0

      actions `shouldBe` [AGetOpenPullRequests, AGetPullRequest prId, ATryPromote integratedSha, ACleanupTestBranch prId]

    it "leaves feedback comment when sync detects build failure" $ do
      let
        prId = PullRequestId 1
        integratedSha = Sha "84c"
        approval = Approval (Username "deckard") Nothing Project.Merge 0 Nothing Normal
        state0 =
          Project.setApproval prId (Just approval) $
            Project.setIntegrationStatus prId (Integrated integratedSha (Project.AnyCheck BuildPending)) $
              insertInitialPr prId Project.emptyProjectState
        results =
          defaultGithubResults
            { resultGetOpenPullRequests = [Just $ IntSet.singleton 1]
            , resultGetBuildStatus = [Just [(Check "build", BuildFailed Nothing)]]
            }
        (_state, actions) =
          runSyncWithHandle results $
            Logic.handleEvent testTriggerConfig (Config.MergeWindowExemptionConfiguration []) Nothing testTimeouts Synchronize state0

      actions `shouldBe` [AGetOpenPullRequests, AGetPullRequest prId, AGetBuildStatus integratedSha, ALeaveComment prId "message"]
