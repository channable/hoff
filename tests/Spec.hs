{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- Hoff
-- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (readFile)
import Data.Foldable (foldlM)
import Data.IntSet (IntSet)
import Data.List (group)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (Text, pack)
import Effectful (Eff, (:>), runPureEff)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Local ( State )
import Effectful.Writer.Static.Local ( Writer )
import Prelude hiding (readFile)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.UUID.V4 as Uuid
import qualified Data.Text as Text
import qualified Effectful.State.Static.Local as State
import qualified Effectful.Writer.Static.Local as Writer

import EventLoop (convertGithubEvent)
import Format (format, Only (..))
import Git (BaseBranch (..), Branch (..), PushResult (..), Sha (..), TagMessage (..), TagName (..),
            GitIntegrationFailure (..), Context (..))
import Github (CommentPayload, CommitStatusPayload, PullRequestPayload)
import Logic (Action, Action (..), Event (..), IntegrationFailure (..), RetrieveEnvironment (..))
import Project (Approval (..), DeployEnvironment (..), ProjectState (ProjectState), PullRequest (PullRequest))
import Types (PullRequestId (..), Username (..))
import ProjectSpec (projectSpec)

import qualified Configuration as Config
import qualified Github
import qualified GithubApi
import qualified Logic
import qualified Project
import qualified WebInterface

import qualified Data.Time as T
import qualified Data.Time.Calendar.OrdinalDate as T
import qualified Data.Set as Set

masterBranch :: BaseBranch
masterBranch = BaseBranch "master"

-- Trigger config used throughout these tests.
testTriggerConfig :: Config.TriggerConfiguration
testTriggerConfig = Config.TriggerConfiguration {
  Config.commentPrefix = "@bot"
}

testProjectConfig :: Config.ProjectConfiguration
testProjectConfig = Config.ProjectConfiguration {
  Config.owner = "peter",
  Config.repository = "rep",
  Config.branch = "master",
  Config.testBranch = "testing",
  Config.checkout = "/var/lib/hoff/checkouts/peter/rep",
  Config.stateFile = "/var/lib/hoff/state/peter/rep.json",
  Config.checks = Just (Config.ChecksConfiguration mempty),
  Config.deployEnvironments = Just ["staging", "production"]
}

testmergeWindowExemptionConfig :: Config.MergeWindowExemptionConfiguration
testmergeWindowExemptionConfig = Config.MergeWindowExemptionConfiguration ["bot"]

-- Functions to prepare certain test states.

singlePullRequestState :: PullRequestId -> Branch -> BaseBranch -> Sha -> Username -> ProjectState
singlePullRequestState pr prBranch baseBranch prSha prAuthor =
  let
    event = PullRequestOpened pr prBranch baseBranch prSha "Untitled" prAuthor
  in
    fst $ runAction $ handleEventTest event Project.emptyProjectState

candidateState
  :: PullRequestId -> Branch -> BaseBranch -> Sha -> Username -> Username -> Sha -> ProjectState
candidateState pr prBranch baseBranch prSha prAuthor approvedBy candidateSha
  = Project.setIntegrationStatus pr (Project.Integrated candidateSha (Project.AnyCheck Project.BuildPending))
  $ Project.setApproval pr (Just (Approval approvedBy Project.Merge 0))
  $ singlePullRequestState pr prBranch baseBranch prSha prAuthor

-- Types and functions to mock running an action without actually doing anything.

data ActionFlat
  = ATryIntegrate
    { mergeMessage         :: Text
    , integrationCandidate :: (PullRequestId, Branch, Sha)
    , mergeTrain           :: [PullRequestId]
    , alwaysAddMergeCommit :: Bool
    }
  | ATryPromote Branch Sha
  | ATryPromoteWithTag Branch Sha TagName TagMessage
  | ALeaveComment PullRequestId Text
  | AIsReviewer Username
  | ACleanupTestBranch PullRequestId
  | AGetPullRequest PullRequestId
  | AGetOpenPullRequests
  deriving (Eq, Show)

-- Results to return from various operations during the tests. There is a
-- default, but specific tests can override some results, to test failure cases.
data Results = Results
  { resultIntegrate           :: [Either IntegrationFailure Sha]
  , resultPush                :: [PushResult]
  , resultGetPullRequest      :: [Maybe GithubApi.PullRequest]
  , resultGetOpenPullRequests :: [Maybe IntSet]
  , resultGetLatestVersion    :: [Either TagName Integer]
  , resultGetChangelog        :: [Maybe Text]
  , resultGetDateTime         :: [T.UTCTime]
  , resultTrainSizeUpdates    :: [Int]
  }

defaultResults :: Results
defaultResults = Results
    -- Pretend that integration always conflicts.
  { resultIntegrate = repeat $ Left $ Logic.IntegrationFailure (BaseBranch "master") MergeFailed
    -- Pretend that pushing is always successful.
  , resultPush = repeat PushOk
    -- Pretend that these two calls to GitHub always fail.
  , resultGetPullRequest = repeat Nothing
  , resultGetOpenPullRequests = repeat Nothing
    -- And pretend that latest version just grows incrementally
  , resultGetLatestVersion = Right <$> [1 ..]
  , resultGetChangelog = repeat Nothing
  , resultGetDateTime = repeat (T.UTCTime (T.fromMondayStartWeek 2021 2 1) (T.secondsToDiffTime 0))
  , resultTrainSizeUpdates = []
  }

-- Consume the head of the field with given getter and setter in the Results.
takeFromList
  :: HasCallStack
  => State Results :> es
  => String
  -> (Results -> [a])
  -> ([a] -> Results -> Results)
  -> Eff es a
takeFromList name getField setField = do
  values <- State.gets getField
  State.modify $ setField $ tail values
  case values of
    []  -> error $ "Not enough results supplied for " <> name <> "."
    v:_ -> pure v

takeResultIntegrate :: (HasCallStack, State Results :> es) => Eff es (Either IntegrationFailure Sha)
takeResultIntegrate =
  takeFromList
    "resultIntegrate"
    resultIntegrate
    (\v res -> res { resultIntegrate = v })

takeResultPush :: (HasCallStack, State Results :> es) => Eff es PushResult
takeResultPush =
  takeFromList
    "resultPush"
    resultPush
    (\v res -> res { resultPush = v })

takeResultGetPullRequest :: (HasCallStack, State Results :> es) => Eff es (Maybe GithubApi.PullRequest)
takeResultGetPullRequest =
  takeFromList
    "resultGetPullRequest"
    resultGetPullRequest
    (\v res -> res { resultGetPullRequest = v })

takeResultGetOpenPullRequests :: (HasCallStack, State Results :> es) => Eff es (Maybe IntSet)
takeResultGetOpenPullRequests =
  takeFromList
    "resultGetOpenPullRequests"
    resultGetOpenPullRequests
    (\v res -> res { resultGetOpenPullRequests = v })

takeResultGetLatestVersion :: (HasCallStack, State Results :> es) => Eff es (Either TagName Integer)
takeResultGetLatestVersion =
  takeFromList
    "resultGetLatestVersion"
    resultGetLatestVersion
    (\v res -> res { resultGetLatestVersion = v })

takeResultGetChangelog :: (HasCallStack, State Results :> es) => Eff es (Maybe Text)
takeResultGetChangelog =
  takeFromList
    "resultGetChangeLog"
    resultGetChangelog
    (\v res -> res { resultGetChangelog = v })

takeResultGetDateTime :: (HasCallStack, State Results :> es) => Eff es T.UTCTime
takeResultGetDateTime =
  takeFromList
    "resultGetDateTime"
    resultGetDateTime
    (\v res -> res { resultGetDateTime = v })

runRetrieveInfo
  :: State Results :> es
  => Config.ProjectConfiguration
  -> Eff (RetrieveEnvironment : es) a
  -> Eff es a
runRetrieveInfo projectConfig = interpret $ \_ -> \case
  GetProjectConfig -> pure projectConfig
  GetDateTime -> takeResultGetDateTime
  GetBaseBranch -> pure $ BaseBranch $ Config.branch testProjectConfig

type ActionResults = [Action, RetrieveEnvironment, State Results, Writer [ActionFlat]]

-- This function simulates running the actions, and returns the final state,
-- together with a list of all actions that would have been performed. Some
-- actions require input from the outside world. Simulating these actions will
-- consume one entry from the `Results` in the state.
runBaseActionResults
  :: (State Results :> es, Writer [ActionFlat] :> es)
  => Eff (Action : es) a
  -> Eff es a
runBaseActionResults =
  let
    -- In the tests, only "deckard" is a reviewer.
    isReviewer username = elem username ["deckard", "bot"]
  in
    interpret $ \_ -> \case
      TryIntegrate msg candidate train alwaysAddMergeCommit' -> do
        Writer.tell [ATryIntegrate msg candidate train alwaysAddMergeCommit']
        takeResultIntegrate
      TryPromote prBranch headSha -> do
        Writer.tell [ATryPromote prBranch headSha]
        takeResultPush
      TryPromoteWithTag prBranch headSha newTag tagMessage -> do
        Writer.tell [ATryPromoteWithTag prBranch headSha newTag tagMessage]
        (Right newTag, ) <$> takeResultPush
      CleanupTestBranch pr -> do
        Writer.tell [ACleanupTestBranch pr]
        pure ()
      LeaveComment pr body  -> do
        Writer.tell [ALeaveComment pr body]
        pure ()
      IsReviewer username -> do
        Writer.tell [AIsReviewer username]
        pure $ isReviewer username
      GetPullRequest pr -> do
        Writer.tell [AGetPullRequest pr]
        takeResultGetPullRequest
      GetOpenPullRequests -> do
        Writer.tell [AGetOpenPullRequests]
        takeResultGetOpenPullRequests
      GetLatestVersion _ -> takeResultGetLatestVersion
      GetChangelog _ _ -> takeResultGetChangelog
      IncreaseMergeMetric -> pure ()
      UpdateTrainSizeMetric n -> do
        results <- State.get
        State.put $ results { resultTrainSizeUpdates = n : resultTrainSizeUpdates results }
        pure ()

runActionEff
  :: (State Results :> es, Writer [ActionFlat] :> es)
  => Config.ProjectConfiguration
  -> Eff (Action : RetrieveEnvironment : es) a
  -> Eff es a
runActionEff config eff = runRetrieveInfo config $ runBaseActionResults eff

-- Simulates running the action. Use the provided results as result for various
-- operations. Results are consumed one by one.

runActionCustom :: Results -> Eff ActionResults a -> (a, [ActionFlat])
runActionCustom results action = runPureEff $ Writer.runWriter $ State.evalState results $ runActionEff testProjectConfig action

runActionCustomResults :: Results -> Eff ActionResults a -> (a, Results, [ActionFlat])
runActionCustomResults results action =
    flatten $ runPureEff $ Writer.runWriter $ State.runState results $ runActionEff testProjectConfig action
  where
    flatten ((a, b), c) = (a, b, c)

runActionCustomConfig :: Config.ProjectConfiguration -> Results -> Eff ActionResults a -> (a, [ActionFlat])
runActionCustomConfig projectConfig results action =
  runPureEff $ Writer.runWriter $ State.evalState results $ runActionEff projectConfig action

-- Simulates running the action with default results.
runAction :: Eff ActionResults a -> (a, [ActionFlat])
runAction = runActionCustom defaultResults

-- Handle an event, then advance the state until a fixed point,
-- and simulate its side effects.
handleEventTest :: (Action :> es, RetrieveEnvironment :> es) => Event -> ProjectState -> Eff es ProjectState
handleEventTest = Logic.handleEvent testTriggerConfig testmergeWindowExemptionConfig

-- Handle events (advancing the state until a fixed point in between) and
-- simulate their side effects.
handleEventsTest :: (Action :> es, RetrieveEnvironment :> es) => [Event] -> ProjectState -> Eff es ProjectState
handleEventsTest events state = foldlM (flip $ Logic.handleEvent testTriggerConfig testmergeWindowExemptionConfig) state events

-- | Like 'classifiedPullRequests' but just with ids.
-- This should match 'WebInterface.ClassifiedPullRequests'
data ClassifiedPullRequestIds = ClassifiedPullRequestIds
  { building
  , failed
  , approved
  , awaiting :: [PullRequestId]
  } deriving (Eq, Show)

-- | Like 'classifiedPullRequests' but just with ids.
classifiedPullRequestIds :: ProjectState -> ClassifiedPullRequestIds
classifiedPullRequestIds state = ClassifiedPullRequestIds
  { building = fst3 <$> WebInterface.building prs
  , failed   = fst3 <$> WebInterface.failed   prs
  , approved = fst3 <$> WebInterface.approved prs
  , awaiting = fst3 <$> WebInterface.awaiting prs
  }
  where
  prs = WebInterface.classifiedPullRequests state
  fst3 (x,_,_) = x

-- Same as 'unfailedIntegratedPullRequests' but paired with the underlying objects.
getIntegrationCandidates :: ProjectState -> [(PullRequestId, PullRequest)]
getIntegrationCandidates state =
  [ (pullRequestId, candidate)
  | pullRequestId <- Project.unfailedIntegratedPullRequests state
  , Just candidate <- [Project.lookupPullRequest pullRequestId state]
  ]

main :: IO ()
main = hspec $ do
  projectSpec
  describe "Logic.handleEvent" $ do

    it "handles PullRequestOpened" $ do
      let event = PullRequestOpened (PullRequestId 3) (Branch "p") masterBranch (Sha "e0f") "title" "lisa"
          state = fst $ runAction $ handleEventTest event Project.emptyProjectState
      state `shouldSatisfy` Project.existsPullRequest (PullRequestId 3)
      let pr = fromJust $ Project.lookupPullRequest (PullRequestId 3) state
      Project.sha pr               `shouldBe` Sha "e0f"
      Project.author pr            `shouldBe` "lisa"
      Project.approval pr          `shouldBe` Nothing
      Project.integrationStatus pr `shouldBe` Project.NotIntegrated

    it "handles PullRequestClosed" $ do
      let event1 = PullRequestOpened (PullRequestId 1) (Branch "p") masterBranch (Sha "abc") "title" "peter"
          event2 = PullRequestOpened (PullRequestId 2) (Branch "q") masterBranch (Sha "def") "title" "jack"
          event3 = PullRequestClosed (PullRequestId 1)
          state  = fst $ runAction $ handleEventsTest [event1, event2, event3] Project.emptyProjectState
      state `shouldSatisfy` not . Project.existsPullRequest (PullRequestId 1)
      state `shouldSatisfy` Project.existsPullRequest (PullRequestId 2)

    it "handles PullRequestEdited" $ do
      let event1 = PullRequestOpened (PullRequestId 1) (Branch "p") (BaseBranch "m") (Sha "abc") "title" "peter"
          event2 = PullRequestEdited (PullRequestId 1) "newTitle" masterBranch
          state = fst $ runAction $ handleEventsTest [event1, event2] Project.emptyProjectState
          pr = fromJust $ Project.lookupPullRequest (PullRequestId 1) state
      Project.title pr      `shouldBe` "newTitle"
      Project.baseBranch pr `shouldBe` masterBranch

    it "handles closing the integration candidate PR" $ do
      let event  = PullRequestClosed (PullRequestId 1)
          state  = candidateState (PullRequestId 1) (Branch "p") masterBranch (Sha "ea0") "frank" "deckard" (Sha "cf4")
          state' = fst $ runAction $ handleEventTest event state
      Project.unfailedIntegratedPullRequests state' `shouldBe` []

    it "does not modify the integration candidate if a different PR was closed" $ do
      let event  = PullRequestClosed (PullRequestId 1)
          state  = candidateState (PullRequestId 2) (Branch "p") masterBranch (Sha "a38") "franz" "deckard" (Sha "ed0")
          state' = fst $ runAction $ handleEventTest event state
      Project.unfailedIntegratedPullRequests state' `shouldBe` [PullRequestId 2]

    it "loses approval after the PR commit has changed" $ do
      let event  = PullRequestCommitChanged (PullRequestId 1) (Sha "def")
          state0 = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "abc") "alice"
          state1 = Project.setApproval (PullRequestId 1) (Just (Approval "hatter" Project.Merge 0)) state0
          state2 = fst $ runAction $ handleEventTest event state1
          pr1    = fromJust $ Project.lookupPullRequest (PullRequestId 1) state1
          pr2    = fromJust $ Project.lookupPullRequest (PullRequestId 1) state2
      Project.approval pr1 `shouldBe` Just (Approval "hatter" Project.Merge 0)
      Project.approval pr2 `shouldBe` Nothing

    it "does not lose approval after the PR commit has changed due to a push we caused" $ do
      let
        state0 = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "abc") "alice"
        state1 = Project.setApproval (PullRequestId 1) (Just (Approval "hatter" Project.Merge 0)) state0
        state2 = Project.setIntegrationStatus (PullRequestId 1) (Project.Integrated (Sha "dc0") (Project.AnyCheck Project.BuildPending)) state1
        state3 = Project.setIntegrationStatus (PullRequestId 1) (Project.Integrated (Sha "dc1") (Project.AnyCheck Project.BuildPending)) state2
        event  = PullRequestCommitChanged (PullRequestId 1) (Sha "dc0")
        stateAfterEvent = fst . runAction . handleEventTest event
      -- The commit changed, but to the sha that is the integration candidate,
      -- so that should not clear approval; we pushed that ourselves.
      stateAfterEvent state2 `shouldBe` state2
      -- Even if in the mean time, we tried to integrate the PR again, and we
      -- now have a different integration candidate, a CommitChanged event with
      -- a past integration candidate should be ignored.
      stateAfterEvent state3 `shouldBe` state3

    it "resets the approval, integration, and build status after the PR commit has changed" $ do
      let
        state1  = candidateState (PullRequestId 1) (Branch "p") masterBranch (Sha "abc") "thomas" "deckard" (Sha "bcd")
        newPush = PullRequestCommitChanged (PullRequestId 1) (Sha "def")
        state2  = fst $ runAction $ handleEventTest newPush state1
        prAt1   = fromJust $ Project.lookupPullRequest (PullRequestId 1) state1
        prAt2   = fromJust $ Project.lookupPullRequest (PullRequestId 1) state2
      Project.approval          prAt1 `shouldBe` Just (Approval "deckard" Project.Merge 0)
      Project.integrationStatus prAt1 `shouldBe` Project.Integrated (Sha "bcd") (Project.AnyCheck Project.BuildPending)
      Project.approval          prAt2 `shouldBe` Nothing
      Project.integrationStatus prAt2 `shouldBe` Project.NotIntegrated

    it "ignores false positive commit changed events" $ do
      let
        -- This commit change is not really a change, therefore we should not
        -- lose the approval status.
        event  = PullRequestCommitChanged (PullRequestId 1) (Sha "000")
        state0 = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "000") "cindy"
        state1 = Project.setApproval (PullRequestId 1) (Just (Approval "daniel" Project.Merge 0)) state0
        (state2, _actions) = runAction $ Logic.proceedUntilFixedPoint state1
        (state3, actions)  = runAction $ handleEventTest event state2
        prAt3 = fromJust $ Project.lookupPullRequest (PullRequestId 1) state3
      state3 `shouldBe` state2
      actions `shouldBe` []
      Project.approval prAt3 `shouldBe` Just (Approval "daniel" Project.Merge 0)

    it "sets approval after a stamp from a reviewer" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "6412ef5") "toby"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approval pr `shouldBe` Just (Approval "deckard" Project.Merge 0)

    it "does not set approval after a stamp from a non-reviewer" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "6412ef5") "toby"
          -- Note: the comment is a valid approval command, but "rachael" is not
          -- marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "rachael" "@bot merge"
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approval pr `shouldBe` Nothing

    it "does not set approval after a comment with the wrong prefix" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "6412ef5") "patrick"
          -- Note: "deckard" is marked as reviewer in the test config, but the
          -- prefix is "@bot ", so none of the comments below should trigger approval.
          event1 = CommentAdded (PullRequestId 1) "deckard" "@hoffbot merge"
          event2 = CommentAdded (PullRequestId 1) "deckard" "LGTM :shipit:"
          event3 = CommentAdded (PullRequestId 1) "deckard" "!merge"
          -- In these cases, the prefix is correct, but the command is wrong.
          event4 = CommentAdded (PullRequestId 1) "deckard" "@botmerge"
          event5 = CommentAdded (PullRequestId 1) "deckard" "@bot, merge"
          state' = fst $ runAction $ handleEventsTest [event1, event2, event3, event4, event5] state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approval pr `shouldBe` Nothing

    it "accepts command comments case-insensitively" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "6412ef5") "sacha"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "@BoT MeRgE"
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approval pr `shouldBe` Just (Approval "deckard" Project.Merge 0)

    it "accepts command at end of other comments" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "6412ef5") "sacha"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "looks good to me, @bot merge"
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approval pr `shouldBe` Just (Approval "deckard" Project.Merge 0)

    it "accepts command at end of other comments if tagged multiple times" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "6412ef5") "sacha"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "@bot looks good to me, @bot merge"
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approval pr `shouldBe` Just (Approval "deckard" Project.Merge 0)

    it "accepts command before comments" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "6412ef5") "sacha"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "@bot merge\nYou did some fine work here."
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approval pr `shouldBe` Just (Approval "deckard" Project.Merge 0)

    it "does not accepts merge command with interleaved comments" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "6412ef5") "sacha"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "@bot foo merge"
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approval pr `shouldBe` Nothing

    it "handles a build status change of the integration candidate" $ do
      let
        event  = BuildStatusChanged (Sha "84c") "default" Project.BuildSucceeded
        state  = candidateState (PullRequestId 1) (Branch "p") masterBranch (Sha "a38") "johanna" "deckard" (Sha "84c")
        state' = fst $ runAction $ handleEventTest event state
        pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.integrationStatus pr `shouldBe` Project.Promoted

    it "ignores a build status change for commits that are not the integration candidate" $ do
      let
        event0 = PullRequestOpened (PullRequestId 2) (Branch "p") masterBranch (Sha "0ad") "title" "harry"
        event1 = BuildStatusChanged (Sha "0ad") "default" Project.BuildSucceeded
        state  = candidateState (PullRequestId 1) (Branch "p") masterBranch (Sha "a38") "harry" "deckard" (Sha "84c")
        state' = fst $ runAction $ handleEventsTest [event0, event1] state
        pr1    = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
        pr2    = fromJust $ Project.lookupPullRequest (PullRequestId 2) state'
      -- Even though the build status changed for "0ad" which is a known commit,
      -- only the build status of the integration candidate can be changed.
      Project.integrationStatus pr1 `shouldBe` Project.Integrated (Sha "84c") (Project.AnyCheck Project.BuildPending)
      Project.integrationStatus pr2 `shouldBe` Project.NotIntegrated

    it "only checks if a comment author is a reviewer for comment commands" $ do
      let
        state = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "a38") "tyrell"
        event0 = CommentAdded (PullRequestId 1) "deckard" "I don't get it, Tyrell"
        event1 = CommentAdded (PullRequestId 1) "deckard" "@bot merge"
        actions0 = snd $ runAction $ handleEventTest event0 state
        actions1 = snd $ runAction $ handleEventTest event1 state
      actions0 `shouldBe` []
      actions1 `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "a38") [] False
        , ALeaveComment (PullRequestId 1)
            "Failed to rebase, please rebase manually using\n\n\
            \    git fetch && git rebase --interactive --autosquash origin/master p"
        ]

    it "notifies approvers about queue position" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "p") masterBranch (Sha "a38") "Add Nexus 7 experiment" (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "s") masterBranch (Sha "dec") "Some PR" (Username "rachael")
          $ Project.insertPullRequest (PullRequestId 3) (Branch "s") masterBranch (Sha "f16") "Another PR" (Username "rachael")
          $ Project.emptyProjectState
        -- Approve pull request in order of ascending id, mark the last PR for deployment.
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 3) "deckard" "@bot merge and deploy"
          ]
        -- For this test, we assume all integrations and pushes succeed.
        results = defaultResults { resultIntegrate = [ Right (Sha "b71")
                                                     , Right (Sha "c82")
                                                     , Right (Sha "d93")
                                                     ] }
        (finalState, actions) = runActionCustom results $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Add Nexus 7 experiment\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "a38") [] False
        , ALeaveComment (PullRequestId 1) "Rebased as b71, waiting for CI …"

        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2) "Pull request approved for merge by @deckard, waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Some PR\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "dec")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as c82 behind 1 other PR, waiting for CI …"

        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 3) "Pull request approved for merge and deploy to staging by @deckard, waiting for rebase behind 2 pull requests."
        , ATryIntegrate "Merge #3: Another PR\n\nApproved-by: deckard\nAuto-deploy: true\nDeploy-Environment: staging\n"
                        (PullRequestId 3, Branch "refs/pull/3/head", Sha "f16")
                        [PullRequestId 1, PullRequestId 2]
                        True
        , ALeaveComment (PullRequestId 3) "Speculatively rebased as d93 behind 2 other PRs, waiting for CI …"
        ]
      classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
        { building = [PullRequestId 1, PullRequestId 2, PullRequestId 3]
        , failed   = []
        , approved = []
        , awaiting = []
        }
    it "keeps the order position for the comments" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "p") masterBranch (Sha "a38") "Add Nexus 7 experiment" (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "s") masterBranch (Sha "dec") "Some PR" (Username "rachael")
          $ Project.insertPullRequest (PullRequestId 3) (Branch "s") masterBranch (Sha "f16") "Another PR" (Username "rachael")
          $ Project.emptyProjectState
        -- Approve pull request in order of ascending id, mark the last PR for deployment.
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 3) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          ]
        -- For this test, we assume all integrations and pushes succeed.
        results = defaultResults { resultIntegrate = [Right (Sha "b71"), Right (Sha "b72"), Right (Sha "b73")] }
        run = runActionCustom results
        (state', actions) = run $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Add Nexus 7 experiment\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "a38") [] False
        , ALeaveComment (PullRequestId 1) "Rebased as b71, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 3) "Pull request approved for merge by @deckard, waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #3: Another PR\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 3, Branch "refs/pull/3/head", Sha "f16") [PullRequestId 1] False
        , ALeaveComment (PullRequestId 3) "Speculatively rebased as b72 behind 1 other PR, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2) "Pull request approved for merge by @deckard, waiting for rebase behind 2 pull requests."
        , ATryIntegrate "Merge #2: Some PR\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "dec") [PullRequestId 1, PullRequestId 3] False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as b73 behind 2 other PRs, waiting for CI …"
        ]
      Project.pullRequestApprovalIndex state' `shouldBe` 3
      Project.pullRequests state' `shouldBe`
        IntMap.fromList
          [ (1, PullRequest {
            sha = Sha "a38",
            branch = Branch "p",
            baseBranch = BaseBranch "master",
            title = "Add Nexus 7 experiment",
            author = Username "tyrell",
            approval = Just (Approval (Username "deckard") Project.Merge 0),
            integrationStatus = Project.Integrated (Sha "b71") (Project.AnyCheck Project.BuildPending),
            integrationAttempts = [],
            needsFeedback = False
            })
          , (2, PullRequest {
            sha = Sha "dec",
            branch = Branch "s",
            baseBranch = BaseBranch "master",
            title = "Some PR",
            author = Username "rachael",
            approval = Just (Approval (Username "deckard") Project.Merge 2),
            integrationStatus = Project.Integrated (Sha "b73") (Project.AnyCheck Project.BuildPending),
            integrationAttempts = [],
            needsFeedback = False
            })
          , (3, PullRequest {
            sha = Sha "f16",
            branch = Branch "s",
            baseBranch = BaseBranch "master",
            title = "Another PR",
            author = Username "rachael",
            approval = Just (Approval (Username "deckard") Project.Merge 1),
            integrationStatus = Project.Integrated (Sha "b72") (Project.AnyCheck Project.BuildPending),
            integrationAttempts = [],
            needsFeedback = False
            })
          ]
      classifiedPullRequestIds state' `shouldBe` ClassifiedPullRequestIds
        { building = [PullRequestId 1, PullRequestId 3, PullRequestId 2]
        , failed   = []
        , approved = []
        , awaiting = []
        }

      -- Approve pull requests, but not in order of ascending id.
      let
        eventsPermuted =
          [ CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 3) "deckard" "@bot merge"
          ]
        actionsPermuted = snd $ run $ handleEventsTest eventsPermuted state
      actionsPermuted `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #2: Some PR\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "dec") [] False
        , ALeaveComment (PullRequestId 2) "Rebased as b71, waiting for CI …"

        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved for merge by @deckard, waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #1: Add Nexus 7 experiment\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "a38") [PullRequestId 2] False
        , ALeaveComment (PullRequestId 1) "Speculatively rebased as b72 behind 1 other PR, waiting for CI …"

        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 3) "Pull request approved for merge by @deckard, waiting for rebase behind 2 pull requests."
        , ATryIntegrate "Merge #3: Another PR\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 3, Branch "refs/pull/3/head", Sha "f16") [PullRequestId 2, PullRequestId 1] False
        , ALeaveComment (PullRequestId 3) "Speculatively rebased as b73 behind 2 other PRs, waiting for CI …"
        ]

    it "abandons integration when a pull request is closed" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "p") masterBranch (Sha "a38") "Add Nexus 7 experiment" (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "s") masterBranch (Sha "dec") "Some PR" (Username "rachael")
          $ Project.emptyProjectState
        -- Approve both pull requests, then close the first.
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , PullRequestClosed (PullRequestId 1)
          ]
        -- For this test, we assume all integrations and pushes succeed.
        results = defaultResults
          { resultIntegrate = [Right (Sha "b71"), Right (Sha "b72"), Right (Sha "b73")] }
        run = runActionCustom results
        (state', actions) = run $ handleEventsTest events state

      -- The first pull request should be dropped, and a comment should be
      -- left indicating why. Then the second pull request should be at the
      -- front of the queue.
      Project.unfailedIntegratedPullRequests state' `shouldBe` [PullRequestId 2]
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Add Nexus 7 experiment\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "a38") [] False
        , ALeaveComment (PullRequestId 1) "Rebased as b71, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2) "Pull request approved for merge by @deckard, waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Some PR\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "dec") [PullRequestId 1] False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as b72 behind 1 other PR, waiting for CI …"
        , ALeaveComment (PullRequestId 1) "Abandoning this pull request because it was closed."
        , ACleanupTestBranch (PullRequestId 1)
        , ATryIntegrate "Merge #2: Some PR\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "dec") [] False
        , ALeaveComment (PullRequestId 2) "Rebased as b73, waiting for CI …"
        ]
      classifiedPullRequestIds state' `shouldBe` ClassifiedPullRequestIds
        { building = [PullRequestId 2]
        , failed   = []
        , approved = []
        , awaiting = []
        }

    it "ignores comments on unknown pull requests" $ do
      let
        -- We comment on PR #1, but the project is empty, so this comment should
        -- be dropped on the floor.
        event = CommentAdded (PullRequestId 1) "deckard" "@bot merge"
        (state, actions) = runAction $ handleEventTest event Project.emptyProjectState
      -- We expect no changes to the state, and in particular, no side effects.
      state `shouldBe` Project.emptyProjectState
      actions `shouldBe` []

    it "checks whether pull requests are still open on synchronize" $ do
      let
        state = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "b7332ba") "tyrell"
        results = defaultResults
          { resultGetOpenPullRequests = [Just $ IntSet.singleton 1]
          }
        (state', actions) = runActionCustom results $ handleEventTest Synchronize state
      -- Pull request 1 is open, so the state should not have changed.
      state' `shouldBe` state
      -- We should have queried GitHub about open pull requests.
      actions `shouldBe` [AGetOpenPullRequests]

    it "closes pull requests that are no longer open on synchronize" $ do
      let
        state = singlePullRequestState (PullRequestId 10) (Branch "p") masterBranch (Sha "b7332ba") "tyrell"
        results = defaultResults
          { resultGetOpenPullRequests = [Just $ IntSet.empty]
          }
        (state', actions) = runActionCustom results $ handleEventTest Synchronize state
      -- No pull requests are open on GitHub, so synchronize should have removed
      -- the single initial PR.
      state' `shouldBe` Project.emptyProjectState
      actions `shouldBe` [AGetOpenPullRequests]

    it "does not modify the state on an error during synchronize" $ do
      let
        state = singlePullRequestState (PullRequestId 19) (Branch "p") masterBranch (Sha "b7332ba") "tyrell"
        -- Set up some custom results where we simulate that the GitHub API fails.
        results = defaultResults
          { resultGetOpenPullRequests = [Nothing]
          }
        (state', actions) = runActionCustom results $ handleEventTest Synchronize state
      -- We should not have modified anything on error.
      state' `shouldBe` state
      actions `shouldBe` [AGetOpenPullRequests]

    it "adds missing pull requests during synchronize" $ do
      let
        state = Project.emptyProjectState
        results = defaultResults
          { resultGetOpenPullRequests = [Just $ IntSet.singleton 17]
          , resultGetPullRequest =
            [ Just $ GithubApi.PullRequest
              { GithubApi.sha        = Sha "7faa52318"
              , GithubApi.branch     = Branch "nexus-7"
              , GithubApi.baseBranch = masterBranch
              , GithubApi.title      = "Add Nexus 7 experiment"
              , GithubApi.author     = Username "tyrell"
              }
            ]
          }
        (state', actions) = runActionCustom results $ handleEventTest Synchronize state
        Just pr17 = Project.lookupPullRequest (PullRequestId 17) state'

      -- PR 17 should have been added, with the details defined above.
      Project.title pr17  `shouldBe` "Add Nexus 7 experiment"
      Project.author pr17 `shouldBe` Username "tyrell"
      Project.branch pr17 `shouldBe` Branch "nexus-7"
      Project.sha pr17    `shouldBe` Sha "7faa52318"

      -- Approval and integration status should be set to their initial values,
      -- we do not go back and scan for approval comments on missing PRs.
      Project.approval pr17            `shouldBe` Nothing
      Project.integrationStatus pr17   `shouldBe` Project.NotIntegrated
      Project.integrationAttempts pr17 `shouldBe` []
      actions `shouldBe` [AGetOpenPullRequests, AGetPullRequest (PullRequestId 17)]

    it "does not query details of existing pull requests on synchronize" $ do
      let
        state = singlePullRequestState (PullRequestId 19) (Branch "p") masterBranch (Sha "b7332ba") "tyrell"
        results = defaultResults
          { resultGetOpenPullRequests = [Just $ IntSet.singleton 19]
          }
        actions = snd $ runActionCustom results $ handleEventTest Synchronize state

      -- We should only obtain pull request details for pull requests that were
      -- missing. In this case, PR 19 was already present, so we should not have
      -- obtained its details.
      actions `shouldBe` [AGetOpenPullRequests]

    it "recognizes 'merge and deploy' commands as the proper ApprovedFor value" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge and deploy"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")] }
        (state', actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Pull request approved for merge and deploy to staging by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: true\nDeploy-Environment: staging\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] True
        , ALeaveComment prId "Rebased as def2345, waiting for CI \x2026"
        ]

      fromJust (Project.lookupPullRequest prId state') `shouldSatisfy`
        (\pr -> Project.approval pr== Just (Approval (Username "deckard") (Project.MergeAndDeploy $ DeployEnvironment "staging") 0))

    it "recognizes 'merge and deploy to <environment>' commands as the proper ApprovedFor value" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge and deploy to production"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")] }
        (state', actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Pull request approved for merge and deploy to production by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: true\nDeploy-Environment: production\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] True
        , ALeaveComment prId "Rebased as def2345, waiting for CI \x2026"
        ]

      fromJust (Project.lookupPullRequest prId state') `shouldSatisfy`
        (\pr -> Project.approval pr== Just (Approval (Username "deckard") (Project.MergeAndDeploy $ DeployEnvironment "production") 0))


    it "ignores the 'deploy' in 'merge and deploy' commands when no deployEnvironments are configured " $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge and deploy"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")] }
        (state', actions) = runActionCustomConfig (testProjectConfig{Config.deployEnvironments = Just []}) results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] False
        , ALeaveComment prId "Rebased as def2345, waiting for CI \x2026"
        ]

      fromJust (Project.lookupPullRequest prId state') `shouldSatisfy`
        (\pr -> Project.approval pr== Just (Approval (Username "deckard") (Project.Merge) 0))

    it "recognizes 'merge and deploy on Friday' commands as the proper ApprovedFor value" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge and deploy on Friday"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")], resultGetDateTime = repeat (T.UTCTime (T.fromMondayStartWeek 2021 2 5) (T.secondsToDiffTime 0))}
        (state', actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Pull request approved for merge and deploy to staging by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: true\nDeploy-Environment: staging\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] True
        , ALeaveComment prId "Rebased as def2345, waiting for CI \x2026"
        ]

      fromJust (Project.lookupPullRequest prId state') `shouldSatisfy`
        (\pr -> Project.approval pr== Just (Approval (Username "deckard") (Project.MergeAndDeploy $ DeployEnvironment "staging") 0))

    it "recognizes 'merge and tag' command" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge and tag"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")] }
        (state', actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Pull request approved for merge and tag by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] False
        , ALeaveComment prId "Rebased as def2345, waiting for CI \x2026"
        ]

      fromJust (Project.lookupPullRequest prId state') `shouldSatisfy`
        (\pr -> Project.approval pr == Just (Approval (Username "deckard") Project.MergeAndTag 0))

    it "recognizes 'merge and  tag' command" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge and  tag"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")] }
        (state', actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Pull request approved for merge and tag by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] False
        , ALeaveComment prId "Rebased as def2345, waiting for CI \x2026"
        ]

      fromJust (Project.lookupPullRequest prId state') `shouldSatisfy`
        (\pr -> Project.approval pr == Just (Approval (Username "deckard") Project.MergeAndTag 0))

    it "recognizes 'merge  and tag' command" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge  and tag"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")] }
        (state', actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Pull request approved for merge and tag by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] False
        , ALeaveComment prId "Rebased as def2345, waiting for CI \x2026"
        ]

      fromJust (Project.lookupPullRequest prId state') `shouldSatisfy`
        (\pr -> Project.approval pr == Just (Approval (Username "deckard") Project.MergeAndTag 0))

    it "recognizes 'merge and tag on friday' command" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge and tag on friday"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")], resultGetDateTime = repeat (T.UTCTime (T.fromMondayStartWeek 2021 2 5) (T.secondsToDiffTime 0)) }
        (state', actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Pull request approved for merge and tag by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] False
        , ALeaveComment prId "Rebased as def2345, waiting for CI \x2026"
        ]

      fromJust (Project.lookupPullRequest prId state') `shouldSatisfy`
        (\pr -> Project.approval pr == Just (Approval (Username "deckard") Project.MergeAndTag 0))

    it "recognizes 'merge' command" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")] }
        (state', actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] False
        , ALeaveComment prId "Rebased as def2345, waiting for CI \x2026"
        ]

      fromJust (Project.lookupPullRequest prId state') `shouldSatisfy`
        (\pr -> Project.approval pr == Just (Approval (Username "deckard") Project.Merge 0))

    it "recognizes 'merge on Friday' command" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge on Friday"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")], resultGetDateTime = repeat (T.UTCTime (T.fromMondayStartWeek 2021 2 5) (T.secondsToDiffTime 0)) }
        (state', actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] False
        , ALeaveComment prId "Rebased as def2345, waiting for CI \x2026"
        ]

      fromJust (Project.lookupPullRequest prId state') `shouldSatisfy`
        (\pr -> Project.approval pr == Just (Approval (Username "deckard") Project.Merge 0))

    it "notifies when command not recognized" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot mergre"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")] }
        (_, actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ ALeaveComment prId "`mergre` was not recognized as a valid command." ]

    it "allow 'merge' on Friday for exempted users" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "bot" "@bot merge"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")], resultGetDateTime = repeat (T.UTCTime (T.fromMondayStartWeek 2021 2 5) (T.secondsToDiffTime 0)) }
        (_, actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "bot"
        , ALeaveComment prId "Pull request approved for merge by @bot, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: bot\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] False
        , ALeaveComment prId "Rebased as def2345, waiting for CI \x2026"
        ]

    it "doesn't allow 'merge and tag' command on Friday" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge and tag"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")], resultGetDateTime = repeat (T.UTCTime (T.fromMondayStartWeek 2021 2 5) (T.secondsToDiffTime 0)) }
        (_, actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Your merge request has been denied, because merging on Fridays is not recommended. To override this behaviour use the command `merge and tag on Friday`."
        ]

    it "doesn't allow 'merge and deploy' command on Friday" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge and deploy"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")], resultGetDateTime = repeat (T.UTCTime (T.fromMondayStartWeek 2021 2 5) (T.secondsToDiffTime 0)) }
        (_, actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Your merge request has been denied, because merging on Fridays is not recommended. To override this behaviour use the command `merge and deploy to staging on Friday`."
        ]

    it "doesn't allow 'merge' command on Friday" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge"

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")], resultGetDateTime = repeat (T.UTCTime (T.fromMondayStartWeek 2021 2 5) (T.secondsToDiffTime 0)) }
        (_, actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Your merge request has been denied, because merging on Fridays is not recommended. To override this behaviour use the command `merge on Friday`."
        ]

    it "refuses to merge an empty rebase" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge"

        results = defaultResults
                { resultIntegrate = [Left (IntegrationFailure (BaseBranch "master") EmptyRebase)]
                }
        (_, actions) = runActionCustom results $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate
            { mergeMessage = "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: false\n"
            , integrationCandidate = (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234")
            , mergeTrain = []
            , alwaysAddMergeCommit = False
            }
        , ALeaveComment (PullRequestId 1)
            "Empty rebase.  Have the changes already been merged into the target branch?  Aborting."
        ]

    it "rejects 'merge' commands to a branch other than master" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") (BaseBranch "m") (Sha "abc1234") "tyrell"

        event = CommentAdded prId "deckard" "@bot merge"

        (state', actions) = runAction $ handleEventTest event state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment prId "Merge rejected: the base branch of this pull request must be set to master. It is currently set to m."
        ]

      fromJust (Project.lookupPullRequest prId state') `shouldSatisfy`
        (\pr -> Project.integrationStatus pr == Project.IncorrectBaseBranch)

    it "doesn't reject 'merge' after a base branch change" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") (BaseBranch "m") (Sha "abc1234") "tyrell"

        events =
          [ CommentAdded prId "deckard" "@bot merge"
          , PullRequestEdited prId "Untitled" masterBranch
          , CommentAdded prId "deckard" "@bot merge"]

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")] }
        (state', actions) = runActionCustom results $ handleEventsTest events state

      actions `shouldBe`
        [ AIsReviewer (Username "deckard")
        , ALeaveComment (PullRequestId 1) "Merge rejected: the base branch of this pull request must be set to master. It is currently set to m."
        , AIsReviewer (Username "deckard")
        , ALeaveComment (PullRequestId 1) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] False
        , ALeaveComment (PullRequestId 1) "Rebased as def2345, waiting for CI \8230"
        ]

      fromJust (Project.lookupPullRequest prId state') `shouldSatisfy`
        (\pr -> Project.integrationStatus pr == Project.Integrated (Sha "def2345") (Project.AnyCheck Project.BuildPending))

    it "loses approval after an invalid base branch change" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        events =
          [ CommentAdded prId "deckard" "@bot merge"
          , PullRequestEdited prId "Untitled" (BaseBranch "m")
          ]

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")] }
        (state', actions) = runActionCustom results $ handleEventsTest events state
        pr = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'

      actions `shouldBe`
        [ AIsReviewer (Username "deckard")
        , ALeaveComment (PullRequestId 1) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] False
        , ALeaveComment (PullRequestId 1) "Rebased as def2345, waiting for CI \8230"
        , ALeaveComment (PullRequestId 1) "Stopping integration because the PR changed after approval."
        , ACleanupTestBranch (PullRequestId 1)
        ]

      Project.approval pr `shouldBe` Nothing
      Project.unfailedIntegratedPullRequests state' `shouldBe` []

    it "shows an appropriate message when the commit is changed on an approved PR" $ do
      let
        prId = PullRequestId 1
        state = singlePullRequestState prId (Branch "p") masterBranch (Sha "abc1234") "tyrell"

        events =
          [ CommentAdded prId "deckard" "@bot merge"
          , PullRequestCommitChanged prId (Sha "Untitled")
          ]

        results = defaultResults { resultIntegrate = [Right (Sha "def2345")] }
        (state', actions) = runActionCustom results $ handleEventsTest events state
        pr = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'

      actions `shouldBe`
        [ AIsReviewer (Username "deckard")
        , ALeaveComment (PullRequestId 1) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "abc1234") [] False
        , ALeaveComment (PullRequestId 1) "Rebased as def2345, waiting for CI \8230"
        , ALeaveComment (PullRequestId 1) "Stopping integration because the PR changed after approval."
        , ACleanupTestBranch (PullRequestId 1)
        ]

      Project.approval pr `shouldBe` Nothing
      Project.unfailedIntegratedPullRequests state' `shouldBe` []


  describe "Logic.proceedUntilFixedPoint" $ do

    it "finds a new candidate" $ do
      let
        state
          = Project.setApproval (PullRequestId 1) (Just (Approval "fred" Project.Merge 0))
          $ singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "f34") "sally"
        results = defaultResults
          { resultIntegrate = [Right (Sha "38c")]
          , resultPush = [PushRejected "test"]
          }
        (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
        [(prId, pullRequest)] = getIntegrationCandidates state'
      Project.integrationStatus pullRequest `shouldBe` Project.Integrated (Sha "38c") (Project.AnyCheck Project.BuildPending)
      prId    `shouldBe` PullRequestId 1
      actions `shouldBe`
        [ ATryIntegrate "Merge #1: Untitled\n\nApproved-by: fred\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "f34") [] False
        , ALeaveComment (PullRequestId 1) "Rebased as 38c, waiting for CI \x2026"
        ]
    it "finds a new candidate with multiple PRs" $ do
      let
        state
          = Project.setApproval (PullRequestId 2) (Just (Approval "fred" Project.Merge 0))
          $ Project.setApproval (PullRequestId 1) (Just (Approval "fred" Project.Merge 1))
          $ fst $ runAction $ handleEventsTest
            [ PullRequestOpened (PullRequestId 1) (Branch "p") masterBranch (Sha "f34") "Untitled" "sally"
            , PullRequestOpened (PullRequestId 2) (Branch "s") masterBranch (Sha "g35") "Another untitled" "rachael"
            ] Project.emptyProjectState
        results = defaultResults
          { resultIntegrate = [Right (Sha "38c"), Right (Sha "49d")]
          , resultPush = [PushRejected "test"]
          }
        (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
        (prId, pullRequest):_ = getIntegrationCandidates state'
      Project.integrationStatus pullRequest `shouldBe` Project.Integrated (Sha "38c") (Project.AnyCheck Project.BuildPending)
      prId    `shouldBe` PullRequestId 2
      actions `shouldBe`
        [ ATryIntegrate "Merge #2: Another untitled\n\nApproved-by: fred\nAuto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "g35") [] False
        , ALeaveComment (PullRequestId 2) "Rebased as 38c, waiting for CI \x2026"
        , ATryIntegrate "Merge #1: Untitled\n\nApproved-by: fred\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "f34") [PullRequestId 2] False
        , ALeaveComment (PullRequestId 1) "Speculatively rebased as 49d behind 1 other PR, waiting for CI \x2026"
        ]

    it "pushes after a successful build" $ do
      let
        pullRequest = PullRequest
          { Project.branch              = Branch "results/rachael"
          , Project.baseBranch          = masterBranch
          , Project.sha                 = Sha "f35"
          , Project.title               = "Add my test results"
          , Project.author              = "rachael"
          , Project.approval            = Just (Approval "deckard" Project.Merge 0)
          , Project.integrationStatus   = Project.Integrated (Sha "38d") (Project.AnyCheck Project.BuildSucceeded)
          , Project.integrationAttempts = []
          , Project.needsFeedback       = False
          }
        state = ProjectState
          { Project.pullRequests         = IntMap.singleton 1 pullRequest
          , Project.pullRequestApprovalIndex = 1
          , Project.mandatoryChecks = mempty
          }
        results = defaultResults { resultIntegrate = [Right (Sha "38e")] }
        (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
        candidates = getIntegrationCandidates state'
      -- After a successful push, the candidate should be gone.
      candidates `shouldBe` []
      actions    `shouldBe` [ ATryPromote (Branch "results/rachael") (Sha "38d")
                            , ACleanupTestBranch (PullRequestId 1)
                            ]

    it "pushes and tags with a new version after a successful build (merge and tag)" $ do
      let
        pullRequest = PullRequest
          { Project.branch              = Branch "results/rachael"
          , Project.baseBranch          = masterBranch
          , Project.sha                 = Sha "f35"
          , Project.title               = "Add my test results"
          , Project.author              = "rachael"
          , Project.approval            = Just (Approval "deckard" Project.MergeAndTag 0)
          , Project.integrationStatus   = Project.Integrated (Sha sha) (Project.AnyCheck Project.BuildSucceeded)
          , Project.integrationAttempts = []
          , Project.needsFeedback       = False
          }
        state = ProjectState
          { Project.pullRequests         = IntMap.singleton 1 pullRequest
          , Project.pullRequestApprovalIndex = 1
          , Project.mandatoryChecks = mempty
          }
        results = defaultResults
          { resultIntegrate = [Right (Sha sha)]
          , resultGetChangelog = [Just "changelog"]
          }
        (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
        candidates = getIntegrationCandidates state'
        sha = "38e"
        tagMessage = format "@deckard I tagged your PR with [v2](https://github.com/{}/{}/releases/tags/v2). "
                            (Config.owner testProjectConfig, Config.repository testProjectConfig)
        commitMessage = format "Please wait for the build of {} to pass and don't forget to deploy it!"
                               (Only { fromOnly = sha })
      -- After a successful push, the candidate should be gone.
      candidates `shouldBe` []
      actions    `shouldBe`
        [ ATryPromoteWithTag (Branch "results/rachael") (Sha sha) (TagName "v2")
            (TagMessage "v2\n\nchangelog")
        , ALeaveComment (PullRequestId 1) $ Text.concat [tagMessage, commitMessage]
        , ACleanupTestBranch (PullRequestId 1)
        ]

    it "pushes and tags with a new version after a successful build (merge and deploy)" $ do
      let
        pullRequest = PullRequest
          { Project.branch              = Branch "results/rachael"
          , Project.baseBranch          = masterBranch
          , Project.sha                 = Sha "f35"
          , Project.title               = "Add my test results"
          , Project.author              = "rachael"
          , Project.approval            = Just (Approval "deckard" (Project.MergeAndDeploy $ DeployEnvironment "staging") 0)
          , Project.integrationStatus   = Project.Integrated (Sha "38d") (Project.AnyCheck Project.BuildSucceeded)
          , Project.integrationAttempts = []
          , Project.needsFeedback       = False
          }
        state = ProjectState
          { Project.pullRequests         = IntMap.singleton 1 pullRequest
          , Project.pullRequestApprovalIndex = 1
          , Project.mandatoryChecks = mempty
          }
        results = defaultResults
          { resultIntegrate = [Right (Sha "38e")]
          , resultGetChangelog = [Just "changelog"]
          }
        (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
        candidates = getIntegrationCandidates state'
        tagMessage = format "@deckard I tagged your PR with [v2](https://github.com/{}/{}/releases/tags/v2). "
                            (Config.owner testProjectConfig, Config.repository testProjectConfig)
      -- After a successful push, the candidate should be gone.
      candidates `shouldBe` []
      actions    `shouldBe`
        [ ATryPromoteWithTag (Branch "results/rachael") (Sha "38d") (TagName "v2")
            (TagMessage "v2 (autodeploy)\n\nchangelog")
        , ALeaveComment (PullRequestId 1) $
            Text.concat [tagMessage, "It is scheduled for autodeploy!"]
        , ACleanupTestBranch (PullRequestId 1)
        ]

    it "pushes after successful build even if tagging failed" $ do
      let
        pullRequest = PullRequest
          { Project.branch              = Branch "results/rachael"
          , Project.baseBranch          = masterBranch
          , Project.sha                 = Sha "f35"
          , Project.title               = "Add my test results"
          , Project.author              = "rachael"
          , Project.approval            = Just (Approval "deckard" Project.MergeAndTag 0)
          , Project.integrationStatus   = Project.Integrated (Sha "38d") (Project.AnyCheck Project.BuildSucceeded)
          , Project.integrationAttempts = []
          , Project.needsFeedback       = False
          }
        state = ProjectState
          { Project.pullRequests         = IntMap.singleton 1 pullRequest
          , Project.pullRequestApprovalIndex = 1
          , Project.mandatoryChecks = mempty
          }
        results = defaultResults { resultIntegrate = [Right (Sha "38e")]
                                 , resultGetLatestVersion = [Left (TagName "abcdef")] }
        (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
        candidates = getIntegrationCandidates state'
      -- After a successful push, the candidate should be gone.
      candidates `shouldBe` []
      actions    `shouldBe` [ ALeaveComment (PullRequestId 1) "@deckard Sorry, I could not tag your PR. The previous tag `abcdef` seems invalid"
                            , ATryPromote (Branch "results/rachael") (Sha "38d")
                            , ACleanupTestBranch (PullRequestId 1)
                            ]


    it "restarts the sequence after a rejected push" $ do
      -- Set up a pull request that has gone through the review and build cycle,
      -- and is ready to be pushed to master.
      let
        pullRequest = PullRequest
          { Project.branch              = Branch "results/rachael"
          , Project.baseBranch          = masterBranch
          , Project.sha                 = Sha "f35"
          , Project.title               = "Add my test results"
          , Project.author              = "rachael"
          , Project.approval            = Just (Approval "deckard" Project.Merge 0)
          , Project.integrationStatus   = Project.Integrated (Sha "38d") (Project.AnyCheck Project.BuildSucceeded)
          , Project.integrationAttempts = []
          , Project.needsFeedback       = False
          }
        state = ProjectState
          { Project.pullRequests         = IntMap.singleton 1 pullRequest
          , Project.pullRequestApprovalIndex = 1
          , Project.mandatoryChecks = mempty
          }
        -- Run 'proceedUntilFixedPoint', and pretend that pushes fail (because
        -- something was pushed in the mean time, for instance).
        results = defaultResults
          { resultIntegrate = [Right (Sha "38e")]
          , resultPush = [PushRejected "test"]
          }
        (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
        [(_, pullRequest')] = getIntegrationCandidates state'

      Project.integrationStatus   pullRequest' `shouldBe` Project.Integrated (Sha "38e") (Project.AnyCheck Project.BuildPending)
      Project.integrationAttempts pullRequest' `shouldBe` [Sha "38d"]
      actions `shouldBe`
        [ ATryPromote (Branch "results/rachael") (Sha "38d")
        , ATryIntegrate "Merge #1: Add my test results\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "f35") [] False
        , ALeaveComment (PullRequestId 1) "Rebased as 38e, waiting for CI \x2026"
        ]

    it "restarts the sequence after a rejected push with tag" $ do
      -- Set up a pull request that has gone through the review and build cycle,
      -- and is ready to be pushed to master.
      let
        pullRequest = PullRequest
          { Project.branch              = Branch "results/rachael"
          , Project.baseBranch          = masterBranch
          , Project.sha                 = Sha "f35"
          , Project.title               = "Add my test results"
          , Project.author              = "rachael"
          , Project.approval            = Just (Approval "deckard" Project.MergeAndTag 0)
          , Project.integrationStatus   = Project.Integrated (Sha "38d") (Project.AnyCheck Project.BuildSucceeded)
          , Project.integrationAttempts = []
          , Project.needsFeedback       = False
          }
        state = ProjectState
          { Project.pullRequests         = IntMap.singleton 1 pullRequest
          , Project.pullRequestApprovalIndex = 1
          , Project.mandatoryChecks          = mempty
          }
        -- Run 'proceedUntilFixedPoint', and pretend that pushes fail (because
        -- something was pushed in the mean time, for instance).
        results = defaultResults
          { resultIntegrate = [Right (Sha "38e")]
          , resultPush = [PushRejected "test"]
          , resultGetChangelog = [Just "changelog"]
          }
        (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
        [(_, pullRequest')] = getIntegrationCandidates state'

      Project.integrationStatus   pullRequest' `shouldBe` Project.Integrated (Sha "38e") (Project.AnyCheck Project.BuildPending)
      Project.integrationAttempts pullRequest' `shouldBe` [Sha "38d"]
      actions `shouldBe`
        [ ATryPromoteWithTag (Branch "results/rachael") (Sha "38d") (TagName "v2") (TagMessage "v2\n\nchangelog")
        , ATryIntegrate "Merge #1: Add my test results\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "f35") [] False
        , ALeaveComment (PullRequestId 1) "Rebased as 38e, waiting for CI \x2026"
        ]

    it "can handle a rebase failure after a failed push" $ do
      -- This is a regression test for the following scenario:
      -- - A pull request goes through the approve-rebase-build cycle.
      -- - Pushing to master fails.
      -- - After restarting the cycle, rebasing fails.
      -- In the past this would trigger an inconsistent state because we forgot
      -- to clear the build status.
      let
        state
          = Project.insertPullRequest
              (PullRequestId 1)
              (Branch "n7")
              masterBranch
              (Sha "a39")
              "Add Nexus 7 experiment"
              (Username "tyrell")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "b71") "default" Project.BuildPending
          , BuildStatusChanged (Sha "b71") "default" Project.BuildSucceeded
          ]
        -- For this test, the first integration succeeds. Then we push, which
        -- fails. Then we try to integrate again, but that fails.
        results = defaultResults
          { resultIntegrate =
            [ Right $ Sha "b71"
            , Left $ Logic.IntegrationFailure masterBranch Git.MergeFailed
            ]
          , resultPush = [ PushRejected "test" ]
          }
        (_state', actions) = runActionCustom results $ handleEventsTest events state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Add Nexus 7 experiment\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "a39") [] False
          -- The first rebase succeeds.
        , ALeaveComment (PullRequestId 1) "Rebased as b71, waiting for CI \x2026"
          -- The first promotion attempt fails
        , ATryPromote (Branch "n7") (Sha "b71")
          -- The second rebase fails.
        , ATryIntegrate "Merge #1: Add Nexus 7 experiment\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "a39") [] False
        , ALeaveComment (PullRequestId 1)
            "Failed to rebase, please rebase manually using\n\n\
            \    git fetch && git rebase --interactive --autosquash origin/master n7"
        ]

    it "picks a new candidate from the queue after a successful push" $ do
      let pullRequest1 = PullRequest
            {
              Project.branch              = Branch "results/leon",
              Project.baseBranch          = masterBranch,
              Project.sha                 = Sha "f35",
              Project.title               = "Add Leon test results",
              Project.author              = "rachael",
              Project.approval            = Just (Approval "deckard" Project.Merge 1),
              Project.integrationStatus   = Project.Integrated (Sha "38d") (Project.AnyCheck Project.BuildSucceeded),
              Project.integrationAttempts = [],
              Project.needsFeedback       = False
            }
          pullRequest2 = PullRequest
            {
              Project.branch              = Branch "results/rachael",
              Project.baseBranch          = masterBranch,
              Project.sha                 = Sha "f37",
              Project.title               = "Add my test results",
              Project.author              = "rachael",
              Project.approval            = Just (Approval "deckard" Project.Merge 0),
              Project.integrationStatus   = Project.NotIntegrated,
              Project.integrationAttempts = [],
              Project.needsFeedback       = False
            }
          prMap = IntMap.fromList [(1, pullRequest1), (2, pullRequest2)]
          -- After a successful push, the state of pull request 1 will still be
          -- BuildSucceeded and Integrated, but the candidate will be Nothing.
          state = ProjectState
            {
              Project.pullRequests         = prMap,
              Project.pullRequestApprovalIndex = 2,
              Project.mandatoryChecks = mempty
            }
          -- Proceeding should pick the next pull request as candidate.
          results = defaultResults { resultIntegrate = [Right (Sha "38e")] }
          (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
          [(cId, _candidate)] = getIntegrationCandidates state'
      cId     `shouldBe` PullRequestId 2
      actions `shouldBe`
        [ ATryPromote (Branch "results/leon") (Sha "38d")
        , ACleanupTestBranch (PullRequestId 1)
        , ATryIntegrate "Merge #2: Add my test results\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "f37") [] False
        , ALeaveComment (PullRequestId 2) "Rebased as 38e, waiting for CI \x2026"
        ]

    it "reports the build status if a user retries the same commit" $ do
      let
        state
          = Project.insertPullRequest
              (PullRequestId 1)
              (Branch "n7")
              masterBranch
              (Sha "a39")
              "Add Nexus 7 experiment"
              (Username "tyrell")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "b71") "default" Project.BuildPending
          , BuildStatusChanged (Sha "b71") "default" (Project.BuildStarted "https://status.example.com/b71")
          , BuildStatusChanged (Sha "b71") "default" $ Project.BuildFailed $ Just $ pack "https://example.com/build-status"
            -- User summons bot again because CI failed for an external reason.
          , CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          -- GitHub notifies Hoff of new comments sent by Hoff:
          , CommentAdded (PullRequestId 1) "bot"
              "The [build failed :x:](https://example.com/build-status).\n\n\
              \If this is the result of a flaky test, close and reopen the PR, then tag me again.  \
              \Otherwise, push a new commit and tag me again."
          , CommentAdded (PullRequestId 1) "bot"
              "The [build failed :x:](https://example.com/build-status).\n\n\
              \If this is the result of a flaky test, close and reopen the PR, then tag me again.  \
              \Otherwise, push a new commit and tag me again."
          ]
        results = defaultResults { resultIntegrate = [Right (Sha "b71")] }
        (state', actions) = runActionCustom results $ handleEventsTest events state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: Add Nexus 7 experiment\n\nApproved-by: deckard\nAuto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "a39") [] False
        , ALeaveComment (PullRequestId 1) "Rebased as b71, waiting for CI \x2026"
        , ALeaveComment (PullRequestId 1) "[CI job :yellow_circle:](https://status.example.com/b71) started."
        , ALeaveComment (PullRequestId 1)
            "The [build failed :x:](https://example.com/build-status).\n\n\
            \If this is the result of a flaky test, close and reopen the PR, then tag me again.  \
            \Otherwise, push a new commit and tag me again."
        , AIsReviewer "deckard"
          -- Nothing has changed for the bot because b71 has already failed, so
          -- it doesn't retry, but reports the correct state.
        , ALeaveComment (PullRequestId 1)
            "The [build failed :x:](https://example.com/build-status).\n\n\
            \If this is the result of a flaky test, close and reopen the PR, then tag me again.  \
            \Otherwise, push a new commit and tag me again."
        ]

      -- the pull request should start and end without needing feedback
      (Project.needsFeedback <$> Project.lookupPullRequest (PullRequestId 1) state)
        `shouldBe` Just False
      (Project.needsFeedback <$> Project.lookupPullRequest (PullRequestId 1) state')
        `shouldBe` Just False

  describe "Github._Payload" $ do

    it "parses a PullRequestPayload correctly" $ do
      examplePayload <- readFile "tests/data/pull-request-payload.json"
      let maybePayload :: Maybe PullRequestPayload
          maybePayload = decode examplePayload
      maybePayload `shouldSatisfy` isJust
      let payload       = fromJust maybePayload
      payload.action     `shouldBe` Github.Opened
      payload.owner      `shouldBe` "baxterthehacker"
      payload.repository `shouldBe` "public-repo"
      payload.number     `shouldBe` 1
      payload.sha        `shouldBe` (Sha "0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c")
      payload.branch     `shouldBe` (Branch "changes")
      payload.baseBranch `shouldBe` masterBranch
      payload.title      `shouldBe` "Update the README with new information"
      payload.author     `shouldBe` "baxterthehacker2"


    it "parses a CommentPayload from a created issue_comment correctly" $ do
      examplePayload <- readFile "tests/data/issue-comment-created-payload.json"
      let maybePayload :: Maybe CommentPayload
          maybePayload = decode examplePayload
      maybePayload `shouldSatisfy` isJust
      let payload       = fromJust maybePayload
      payload.action        `shouldBe` Left Github.CommentCreated
      payload.owner         `shouldBe` "baxterthehacker"
      payload.repository    `shouldBe` "public-repo"
      payload.number        `shouldBe` 2
      payload.author        `shouldBe` "baxterthehacker2"
      payload.body          `shouldBe` "You are totally right! I'll get this fixed right away."

    it "parses a CommentPayload from an edited issue_comment correctly" $ do
      examplePayload <- readFile "tests/data/issue-comment-edited-payload.json"
      let maybePayload :: Maybe CommentPayload
          maybePayload = decode examplePayload
      maybePayload `shouldSatisfy` isJust
      let payload       = fromJust maybePayload
      payload.action        `shouldBe` Left Github.CommentEdited
      payload.owner         `shouldBe` "crtschin"
      payload.repository    `shouldBe` "test"
      payload.number        `shouldBe` 1
      payload.author        `shouldBe` "crtschin"
      payload.body          `shouldBe` "This is an edit of a comment on the issue page."

    it "parses a CommentPayload from a submitted pull_request_review correctly" $ do
      examplePayload <- readFile "tests/data/pull-request-review-submitted-payload.json"
      let maybePayload :: Maybe CommentPayload
          maybePayload = decode examplePayload
      maybePayload `shouldSatisfy` isJust
      let payload       = fromJust maybePayload
      payload.action        `shouldBe` Right Github.ReviewSubmitted
      payload.owner         `shouldBe` "crtschin"
      payload.repository    `shouldBe` "test"
      payload.number        `shouldBe` 1
      payload.author        `shouldBe` "crtschin"
      payload.body          `shouldBe` "This is the finalization comment on the pull request review page."

    it "parses a CommentPayload from a edited pull_request_review correctly" $ do
      examplePayload <- readFile "tests/data/pull-request-review-edited-payload.json"
      let maybePayload :: Maybe CommentPayload
          maybePayload = decode examplePayload
      maybePayload `shouldSatisfy` isJust
      let payload       = fromJust maybePayload
      payload.action        `shouldBe` Right Github.ReviewEdited
      payload.owner         `shouldBe` "crtschin"
      payload.repository    `shouldBe` "test"
      payload.number        `shouldBe` 1
      payload.author        `shouldBe` "crtschin"
      payload.body          `shouldBe` "This is an edit of the finalization comment of the review on the issue page."

    it "parses a CommitStatusPayload correctly" $ do
      examplePayload <- readFile "tests/data/status-payload.json"
      let maybePayload :: Maybe CommitStatusPayload
          maybePayload = decode examplePayload
      maybePayload `shouldSatisfy` isJust
      let payload       = fromJust maybePayload
      payload.owner      `shouldBe` "baxterthehacker"
      payload.repository `shouldBe` "public-repo"
      payload.status     `shouldBe` Github.Success
      payload.url        `shouldBe` Nothing
      payload.sha        `shouldBe` (Sha "9049f1265b7d61be4a8904a9a27120d2064dab3b")

  describe "Configuration" $ do

    it "loads the example config file correctly" $ do
      -- This test loads the example configuration file that is packaged. It
      -- serves two purposes: to test that loading a config file works, but
      -- mainly, to enforce that the example file is kept up to date.
      Right cfg <- Config.loadConfiguration "package/example-config.json"
      Config.secret      cfg `shouldBe` "run 'head --bytes 32 /dev/urandom | base64' and paste output here"
      Config.accessToken cfg `shouldBe` "paste a personal access token for a bot user here"
      Config.port        cfg `shouldBe` 1979
      Config.tls         cfg `shouldSatisfy` isNothing

      let
        projects = Config.projects cfg
        project  = head projects
        user     = Config.user cfg
        trigger  = Config.trigger cfg
        (Config.MergeWindowExemptionConfiguration mergeWindowExemption) = Config.mergeWindowExemption cfg

      Config.owner      project `shouldBe` "your-github-username-or-organization"
      Config.repository project `shouldBe` "your-repo"
      Config.branch     project `shouldBe` "master"
      Config.testBranch project `shouldBe` "testing"
      Config.checkout   project `shouldBe` "/var/lib/hoff/checkouts/your-username/your-repo"
      Config.stateFile  project `shouldBe` "/var/lib/hoff/state/your-username/your-repo.json"

      Config.name user          `shouldBe` "CI Bot"
      Config.email user         `shouldBe` "cibot@example.com"
      Config.sshConfigFile user `shouldBe` "/etc/hoff/ssh_config"

      mergeWindowExemption `shouldBe` ["hoffbot"]
      Config.commentPrefix trigger `shouldBe` "@hoffbot"

  describe "EventLoop.convertGithubEvent" $ do

    let testPullRequestPayload action = Github.PullRequestPayload
          { action     = action
          , owner      = "deckard"
          , repository = "repo"
          , number     = 1
          , branch     = Branch "results"
          , baseBranch = masterBranch
          , sha        = Sha "b26354"
          , title      = "Add test results"
          , author     = "rachael"
          }

    it "converts a pull request opened event" $ do
      let payload = testPullRequestPayload Github.Opened
          Just event = convertGithubEvent $ Github.PullRequest payload
      event `shouldBe`
        (PullRequestOpened (PullRequestId 1) (Branch "results") masterBranch (Sha "b26354") "Add test results" "rachael")

    it "converts a pull request reopened event" $ do
      let payload = testPullRequestPayload Github.Reopened
          Just event = convertGithubEvent $ Github.PullRequest payload
      -- Reopened is treated just like opened, there is no memory in the system.
      event `shouldBe`
        (PullRequestOpened (PullRequestId 1) (Branch "results") masterBranch (Sha "b26354") "Add test results" "rachael")

    it "converts a pull request closed event" $ do
      let payload = testPullRequestPayload Github.Closed
          Just event = convertGithubEvent $ Github.PullRequest payload
      event `shouldBe` (PullRequestClosed (PullRequestId 1))

    it "converts a pull request synchronize event" $ do
      let payload = testPullRequestPayload Github.Synchronize
          Just event = convertGithubEvent $ Github.PullRequest payload
      event `shouldBe` (PullRequestCommitChanged (PullRequestId 1) (Sha "b26354"))

    let testCommentPayload action = Github.CommentPayload
          { action     = action
          , owner      = "rachael"
          , repository = "owl"
          , number     = 1
          , author     = "deckard"
          , body       = "Must be expensive."
          }

    it "converts a comment created event" $ do
      let payload = testCommentPayload $ Left Github.CommentCreated
          Just event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` (CommentAdded (PullRequestId 1) "deckard" "Must be expensive.")

    it "converts a review submitted event" $ do
      let payload = testCommentPayload $ Right Github.ReviewSubmitted
          Just event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` (CommentAdded (PullRequestId 1) "deckard" "Must be expensive.")

    it "ignores a comment edited event" $ do
      let payload = testCommentPayload $ Left Github.CommentEdited
          event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` Nothing

    it "ignores a comment deleted event" $ do
      let payload = testCommentPayload $ Left Github.CommentDeleted
          event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` Nothing

    it "ignores a review edited event" $ do
      let payload = testCommentPayload $ Right Github.ReviewEdited
          event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` Nothing

    it "ignores a review dismissed event" $ do
      let payload = testCommentPayload $ Right Github.ReviewDismissed
          event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` Nothing

    let testCommitStatusPayload status = Github.CommitStatusPayload
          { owner      = "rachael"
          , repository = "owl"
          , status     = status
          , url        = Just "https://travis-ci.org/rachael/owl/builds/1982"
          , sha        = Sha "b26354"
          , context    = Context "default"
          }

    it "converts a commit status pending event" $ do
      let payload = testCommitStatusPayload Github.Pending
          Just event = convertGithubEvent $ Github.CommitStatus payload
      event `shouldBe` (BuildStatusChanged (Sha "b26354") "default" (Project.BuildStarted "https://travis-ci.org/rachael/owl/builds/1982"))

    it "converts a commit status success event" $ do
      let payload = testCommitStatusPayload Github.Success
          Just event = convertGithubEvent $ Github.CommitStatus payload
      event `shouldBe` (BuildStatusChanged (Sha "b26354") "default" Project.BuildSucceeded)

    it "converts a commit status failure event" $ do
      let payload = testCommitStatusPayload Github.Failure
          Just event = convertGithubEvent $ Github.CommitStatus payload
      event `shouldBe` (BuildStatusChanged (Sha "b26354") "default" $ Project.BuildFailed $ Just $ pack "https://travis-ci.org/rachael/owl/builds/1982")

    it "converts a commit status error event" $ do
      let payload = testCommitStatusPayload Github.Error
          Just event = convertGithubEvent $ Github.CommitStatus payload
      -- The error and failure statuses are both converted to "failed".
      event `shouldBe` (BuildStatusChanged (Sha "b26354") "default" $ Project.BuildFailed $ Just $ pack "https://travis-ci.org/rachael/owl/builds/1982")

  describe "ProjectState" $ do

    it "can be restored exactly after roundtripping through json" $ do
      let
        emptyState  = Project.emptyProjectState
        singleState = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "071") "deckard"
        candState   = candidateState (PullRequestId 2) (Branch "p") masterBranch (Sha "073") "rachael" "tyrell" (Sha "079")
        Just emptyState'  = decode $ encode emptyState
        Just singleState' = decode $ encode singleState
        Just candState'   = decode $ encode candState
      emptyState  `shouldBe` emptyState'
      singleState `shouldBe` singleState'
      candState   `shouldBe` candState'

    it "loads correctly after persisting to disk" $ do
      -- Generate a random filename in /tmp (or equivalent) to persist the state
      -- to during the test.
      uuid       <- Uuid.nextRandom
      tmpBaseDir <- getTemporaryDirectory
      let fname = tmpBaseDir </> ("state-" ++ (show uuid) ++ ".json")
          state = singlePullRequestState (PullRequestId 1) (Branch "p") masterBranch (Sha "071") "deckard"
      Project.saveProjectState fname state
      Right state' <- Project.loadProjectState fname
      state `shouldBe` state'
      removeFile fname

    it "ignore build status changes where only the URL is changed" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 12)
              (Branch "tth") masterBranch (Sha "12a") "Twelfth PR"  (Username "person")
          $ Project.emptyProjectState
        results = defaultResults {resultIntegrate = [Right (Sha "1b2")]}
        events =
          [ CommentAdded (PullRequestId 12) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 12) "bot" "Pull request approved for merge, rebasing now."
          , CommentAdded (PullRequestId 12) "bot" "Rebased as 1b2, waiting for CI …"
          , BuildStatusChanged (Sha "1b2") "default" (Project.BuildStarted "example.com/1b2")
          , BuildStatusChanged (Sha "1b2") "default" (Project.BuildStarted "example.com/alt1/1b2")
          , BuildStatusChanged (Sha "1b2") "default" (Project.BuildStarted "example.com/alt2/1b2")
          ]
        actions = snd $ runActionCustom results $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer (Username "deckard")
        , ALeaveComment (PullRequestId 12) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #12: Twelfth PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 12,Branch "refs/pull/12/head",Sha "12a")
                        []
                        False
        , ALeaveComment (PullRequestId 12) "Rebased as 1b2, waiting for CI …"
        , ALeaveComment (PullRequestId 12) "[CI job :yellow_circle:](example.com/1b2) started."
        ]

    it "build failures cannot be superseded by other statuses" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 12)
              (Branch "tth") masterBranch (Sha "12a") "Twelfth PR"  (Username "person")
          $ Project.emptyProjectState
        results = defaultResults {resultIntegrate = [Right (Sha "1b2")]}
        events =
          [ CommentAdded (PullRequestId 12) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 12) "bot" "Pull request approved for merge, rebasing now."
          , CommentAdded (PullRequestId 12) "bot" "Rebased as 1b2, waiting for CI …"
          , BuildStatusChanged (Sha "1b2") "default" (Project.BuildStarted "example.com/1b2")
          , CommentAdded (PullRequestId 1) "bot" "[CI job :yellow_circle:](example.com/1b2) started."
          , BuildStatusChanged (Sha "1b2") "default" (Project.BuildFailed (Just "example.com/1b2"))
          , BuildStatusChanged (Sha "1b2") "default" Project.BuildPending -- ignored
          , BuildStatusChanged (Sha "1b2") "default" (Project.BuildStarted "example.com/1b2") -- ignored
          , BuildStatusChanged (Sha "1b2") "default" (Project.BuildFailed (Just "example.com/alt/1b2")) --ignored
          , BuildStatusChanged (Sha "1b2") "default" Project.BuildSucceeded -- ignored
          ]
        actions = snd $ runActionCustom results $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer (Username "deckard")
        , ALeaveComment (PullRequestId 12) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #12: Twelfth PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 12,Branch "refs/pull/12/head",Sha "12a")
                        []
                        False
        , ALeaveComment (PullRequestId 12) "Rebased as 1b2, waiting for CI …"
        , ALeaveComment (PullRequestId 12) "[CI job :yellow_circle:](example.com/1b2) started."
        , ALeaveComment (PullRequestId 12) "The [build failed :x:](example.com/1b2).\n\n\
                                           \If this is the result of a flaky test, \
                                           \close and reopen the PR, then tag me again.  \
                                           \Otherwise, push a new commit and tag me again."
        ]

    it "build successes cannot be superseded by other statuses" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 12)
              (Branch "tth") masterBranch (Sha "12a") "Twelfth PR"  (Username "person")
          $ Project.emptyProjectState
        results = defaultResults {resultIntegrate = [Right (Sha "1b2")]}
        events =
          [ CommentAdded (PullRequestId 12) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 12) "bot" "Pull request approved for merge, rebasing now."
          , CommentAdded (PullRequestId 12) "bot" "Rebased as 1b2, waiting for CI …"
          , BuildStatusChanged (Sha "1b2") "default" (Project.BuildStarted "example.com/1b2")
          , CommentAdded (PullRequestId 1) "bot" "[CI job :yellow_circle:](example.com/1b2) started."
          , BuildStatusChanged (Sha "1b2") "default" Project.BuildSucceeded
          , BuildStatusChanged (Sha "1b2") "default" Project.BuildPending -- ignored
          , BuildStatusChanged (Sha "1b2") "default" (Project.BuildStarted "example.com/1b2") -- ignored
          , BuildStatusChanged (Sha "1b2") "default" (Project.BuildFailed (Just "example.com/1b2")) -- ignored
          ]
        (finalState, actions) = runActionCustom results $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer (Username "deckard")
        , ALeaveComment (PullRequestId 12) "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #12: Twelfth PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 12,Branch "refs/pull/12/head",Sha "12a")
                        []
                        False
        , ALeaveComment (PullRequestId 12) "Rebased as 1b2, waiting for CI …"
        , ALeaveComment (PullRequestId 12) "[CI job :yellow_circle:](example.com/1b2) started."
        , ATryPromote (Branch "tth") (Sha "1b2")
        , ACleanupTestBranch (PullRequestId 12)
        ]
      -- test caveat, in reality, when Promote works,
      -- the PR is removed from the building list.
      classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
        { building = [PullRequestId 12] -- not here in a real scenario
        , failed   = []
        , approved = []
        , awaiting = []
        }

    context "mandatory checks" $ do
      it "does not unintegrate on failing non-mandatory check" $ do
        let
          projectState = Project.emptyProjectState
            { Project.mandatoryChecks = Project.MandatoryChecks (Set.singleton "required") }
          state
            = Project.insertPullRequest (PullRequestId 12)
                (Branch "tth") masterBranch (Sha "12a") "Twelfth PR"  (Username "person")
            $ Project.insertPullRequest (PullRequestId 13)
                (Branch "ttg") masterBranch (Sha "13a") "Thirteenth PR"  (Username "bob")
            $ projectState
          results = defaultResults {resultIntegrate = [Right (Sha "1b2"), Right (Sha "1b3")]}
          events =
            [ CommentAdded (PullRequestId 12) "deckard" "@bot merge"
            , CommentAdded (PullRequestId 13) "deckard" "@bot merge"
            , CommentAdded (PullRequestId 12) "bot" "Pull request approved for merge, rebasing now."
            , CommentAdded (PullRequestId 12) "bot" "Rebased as 1b2, waiting for CI …"
            , CommentAdded (PullRequestId 13) "bot" "Pull request approved for merge, rebasing now."
            , CommentAdded (PullRequestId 13) "bot" "Rebased as 1b2, waiting for CI …"
            , BuildStatusChanged (Sha "1b2") "first" (Project.BuildStarted "example.com/1b2")
            , BuildStatusChanged (Sha "1b2") "required" (Project.BuildStarted "example.com/required/1b2")
            , CommentAdded (PullRequestId 1) "bot" "[CI job :yellow_circle:](example.com/required/1b2) started."
            , BuildStatusChanged (Sha "1b2") "first" (Project.BuildFailed Nothing)
            , BuildStatusChanged (Sha "1b2") "required" Project.BuildSucceeded
            , BuildStatusChanged (Sha "1b3") "required" Project.BuildSucceeded
            ]
          (finalState, actions) = runActionCustom results $ handleEventsTest events state
        actions `shouldBe`
          [ AIsReviewer (Username "deckard")
          , ALeaveComment (PullRequestId 12) "Pull request approved for merge by @deckard, rebasing now."
          , ATryIntegrate "Merge #12: Twelfth PR\n\n\
                          \Approved-by: deckard\n\
                          \Auto-deploy: false\n"
                          (PullRequestId 12,Branch "refs/pull/12/head",Sha "12a")
                          []
                          False
          , ALeaveComment (PullRequestId 12) "Rebased as 1b2, waiting for CI …"
          , AIsReviewer (Username "deckard")
          , ALeaveComment (PullRequestId 13)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
          , ATryIntegrate "Merge #13: Thirteenth PR\n\n\
                          \Approved-by: deckard\n\
                          \Auto-deploy: false\n"
                          (PullRequestId 13,Branch "refs/pull/13/head",Sha "13a")
                          [PullRequestId 12]
                          False
          , ALeaveComment (PullRequestId 13) "Speculatively rebased as 1b3 behind 1 other PR, waiting for CI …"
          , ALeaveComment (PullRequestId 12) "[CI job :yellow_circle:](example.com/required/1b2) started."
          , ATryPromote (Branch "tth") (Sha "1b2")
          , ACleanupTestBranch (PullRequestId 12)
          , ATryPromote (Branch "ttg") (Sha "1b3")
          , ACleanupTestBranch (PullRequestId 13)
          ]
        -- test caveat, in reality, when Promote works,
        -- the PR is removed from the building list.
        classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
          { building = [PullRequestId 12, PullRequestId 13] -- not here in a real scenario
          , failed   = []
          , approved = []
          , awaiting = []
          }

      it "ignores failing non-mandatory check but succeeding mandatory one" $ do
        let
          projectState = Project.emptyProjectState
            { Project.mandatoryChecks = Project.MandatoryChecks (Set.singleton "required") }
          state
            = Project.insertPullRequest (PullRequestId 12)
                (Branch "tth") masterBranch (Sha "12a") "Twelfth PR"  (Username "person")
            $ projectState
          results = defaultResults {resultIntegrate = [Right (Sha "1b2")]}
          events =
            [ CommentAdded (PullRequestId 12) "deckard" "@bot merge"
            , CommentAdded (PullRequestId 12) "bot" "Pull request approved for merge, rebasing now."
            , CommentAdded (PullRequestId 12) "bot" "Rebased as 1b2, waiting for CI …"
            , BuildStatusChanged (Sha "1b2") "first" (Project.BuildStarted "example.com/1b2")
            , BuildStatusChanged (Sha "1b2") "required" (Project.BuildStarted "example.com/required/1b2")
            , CommentAdded (PullRequestId 1) "bot" "[CI job :yellow_circle:](example.com/required/1b2) started."
            , BuildStatusChanged (Sha "1b2") "first" (Project.BuildFailed Nothing)
            , BuildStatusChanged (Sha "1b2") "required" Project.BuildSucceeded
            ]
          (finalState, actions) = runActionCustom results $ handleEventsTest events state
        actions `shouldBe`
          [ AIsReviewer (Username "deckard")
          , ALeaveComment (PullRequestId 12) "Pull request approved for merge by @deckard, rebasing now."
          , ATryIntegrate "Merge #12: Twelfth PR\n\n\
                          \Approved-by: deckard\n\
                          \Auto-deploy: false\n"
                          (PullRequestId 12,Branch "refs/pull/12/head",Sha "12a")
                          []
                          False
          , ALeaveComment (PullRequestId 12) "Rebased as 1b2, waiting for CI …"
          , ALeaveComment (PullRequestId 12) "[CI job :yellow_circle:](example.com/required/1b2) started."
          , ATryPromote (Branch "tth") (Sha "1b2")
          , ACleanupTestBranch (PullRequestId 12)
          ]
        -- test caveat, in reality, when Promote works,
        -- the PR is removed from the building list.
        classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
          { building = [PullRequestId 12] -- not here in a real scenario
          , failed   = []
          , approved = []
          , awaiting = []
          }

      it "does not ignore failing mandatory check even when succeeding non-mandatory one" $ do
        let
          projectState = Project.emptyProjectState
            { Project.mandatoryChecks = Project.MandatoryChecks (Set.singleton "required") }
          state
            = Project.insertPullRequest (PullRequestId 12)
                (Branch "tth") masterBranch (Sha "12a") "Twelfth PR"  (Username "person")
            $ projectState
          results = defaultResults {resultIntegrate = [Right (Sha "1b2")]}
          events =
            [ CommentAdded (PullRequestId 12) "deckard" "@bot merge"
            , CommentAdded (PullRequestId 12) "bot" "Pull request approved for merge, rebasing now."
            , CommentAdded (PullRequestId 12) "bot" "Rebased as 1b2, waiting for CI …"
            , BuildStatusChanged (Sha "1b2") "first" (Project.BuildStarted "example.com/1b2")
            , BuildStatusChanged (Sha "1b2") "required" (Project.BuildStarted "example.com/required/1b2")
            , CommentAdded (PullRequestId 1) "bot" "[CI job :yellow_circle:](example.com/required/1b2) started."
            , BuildStatusChanged (Sha "1b2") "first" Project.BuildSucceeded
            , BuildStatusChanged (Sha "1b2") "required" (Project.BuildFailed Nothing)
            ]
          (finalState, actions) = runActionCustom results $ handleEventsTest events state
        actions `shouldBe`
          [ AIsReviewer (Username "deckard")
          , ALeaveComment (PullRequestId 12) "Pull request approved for merge by @deckard, rebasing now."
          , ATryIntegrate "Merge #12: Twelfth PR\n\n\
                          \Approved-by: deckard\n\
                          \Auto-deploy: false\n"
                          (PullRequestId 12,Branch "refs/pull/12/head",Sha "12a")
                          []
                          False
          , ALeaveComment (PullRequestId 12) "Rebased as 1b2, waiting for CI …"
          , ALeaveComment (PullRequestId 12) "[CI job :yellow_circle:](example.com/required/1b2) started."
          , ALeaveComment (PullRequestId 12) "The build failed :x:.\n\n\
                                            \If this is the result of a flaky test, \
                                            \close and reopen the PR, then tag me again.  \
                                            \Otherwise, push a new commit and tag me again."
          ]
        -- test caveat, in reality, when Promote works,
        -- the PR is removed from the building list.
        classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
          { building = [] -- not here in a real scenario
          , failed   = [PullRequestId 12]
          , approved = []
          , awaiting = []
          }
      it "does not merge when succeeding non-mandatory checks" $ do
        let
          projectState = Project.emptyProjectState
            { Project.mandatoryChecks = Project.MandatoryChecks (Set.singleton "required") }
          state
            = Project.insertPullRequest (PullRequestId 12)
                (Branch "tth") masterBranch (Sha "12a") "Twelfth PR"  (Username "person")
            $ projectState
          results = defaultResults {resultIntegrate = [Right (Sha "1b2")]}
          events =
            [ CommentAdded (PullRequestId 12) "deckard" "@bot merge"
            , CommentAdded (PullRequestId 12) "bot" "Pull request approved for merge, rebasing now."
            , CommentAdded (PullRequestId 12) "bot" "Rebased as 1b2, waiting for CI …"
            , BuildStatusChanged (Sha "1b2") "first" (Project.BuildStarted "example.com/1b2")
            , BuildStatusChanged (Sha "1b2") "required" (Project.BuildStarted "example.com/required/1b2")
            , CommentAdded (PullRequestId 1) "bot" "[CI job :yellow_circle:](example.com/required/1b2) started."
            , BuildStatusChanged (Sha "1b2") "first" Project.BuildSucceeded
            ]
          (finalState, actions) = runActionCustom results $ handleEventsTest events state
        actions `shouldBe`
          [ AIsReviewer (Username "deckard")
          , ALeaveComment (PullRequestId 12) "Pull request approved for merge by @deckard, rebasing now."
          , ATryIntegrate "Merge #12: Twelfth PR\n\n\
                          \Approved-by: deckard\n\
                          \Auto-deploy: false\n"
                          (PullRequestId 12,Branch "refs/pull/12/head",Sha "12a")
                          []
                          False
          , ALeaveComment (PullRequestId 12) "Rebased as 1b2, waiting for CI …"
          , ALeaveComment (PullRequestId 12) "[CI job :yellow_circle:](example.com/required/1b2) started."
          ]
        -- test caveat, in reality, when Promote works,
        -- the PR is removed from the building list.
        classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
          { building = [PullRequestId 12] -- not here in a real scenario
          , failed   = []
          , approved = []
          , awaiting = []
          }

      it "does merge when succeeding mandatory checks" $ do
        let
          projectState = Project.emptyProjectState
            { Project.mandatoryChecks = Project.MandatoryChecks (Set.singleton "required") }
          state
            = Project.insertPullRequest (PullRequestId 12)
                (Branch "tth") masterBranch (Sha "12a") "Twelfth PR"  (Username "person")
            $ projectState
          results = defaultResults {resultIntegrate = [Right (Sha "1b2")]}
          events =
            [ CommentAdded (PullRequestId 12) "deckard" "@bot merge"
            , CommentAdded (PullRequestId 12) "bot" "Pull request approved for merge, rebasing now."
            , CommentAdded (PullRequestId 12) "bot" "Rebased as 1b2, waiting for CI …"
            , BuildStatusChanged (Sha "1b2") "first" (Project.BuildStarted "example.com/1b2")
            , BuildStatusChanged (Sha "1b2") "required" (Project.BuildStarted "example.com/required/1b2")
            , CommentAdded (PullRequestId 1) "bot" "[CI job :yellow_circle:](example.com/required/1b2) started."
            , BuildStatusChanged (Sha "1b2") "first" Project.BuildSucceeded
            , BuildStatusChanged (Sha "1b2") "required" Project.BuildSucceeded
            ]
          (finalState, actions) = runActionCustom results $ handleEventsTest events state
        actions `shouldBe`
          [ AIsReviewer (Username "deckard")
          , ALeaveComment (PullRequestId 12) "Pull request approved for merge by @deckard, rebasing now."
          , ATryIntegrate "Merge #12: Twelfth PR\n\n\
                          \Approved-by: deckard\n\
                          \Auto-deploy: false\n"
                          (PullRequestId 12,Branch "refs/pull/12/head",Sha "12a")
                          []
                          False
          , ALeaveComment (PullRequestId 12) "Rebased as 1b2, waiting for CI …"
          , ALeaveComment (PullRequestId 12) "[CI job :yellow_circle:](example.com/required/1b2) started."
          , ATryPromote (Branch "tth") (Sha "1b2")
          , ACleanupTestBranch (PullRequestId 12)
          ]
        -- test caveat, in reality, when Promote works,
        -- the PR is removed from the building list.
        classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
          { building = [PullRequestId 12] -- not here in a real scenario
          , failed   = []
          , approved = []
          , awaiting = []
          }

      it "only merges when all mandatory checks pass" $ do
        let
          projectState = Project.emptyProjectState
            { Project.mandatoryChecks = Project.MandatoryChecks (Set.fromList ["required", "mandatory"]) }
          state
            = Project.insertPullRequest (PullRequestId 12)
                (Branch "tth") masterBranch (Sha "12a") "Twelfth PR"  (Username "person")
            $ projectState
          results = defaultResults {resultIntegrate = [Right (Sha "1b2")]}
          events =
            [ CommentAdded (PullRequestId 12) "deckard" "@bot merge"
            , CommentAdded (PullRequestId 12) "bot" "Pull request approved for merge, rebasing now."
            , CommentAdded (PullRequestId 12) "bot" "Rebased as 1b2, waiting for CI …"
            , BuildStatusChanged (Sha "1b2") "required" (Project.BuildStarted "example.com/required/1b2")
            , BuildStatusChanged (Sha "1b2") "mandatory" (Project.BuildStarted "example.com/mandatory/1b2")
            , CommentAdded (PullRequestId 1) "bot" "[CI job :yellow_circle:](example.com/required/1b2) started."
            , BuildStatusChanged (Sha "1b2") "required" Project.BuildSucceeded
            , BuildStatusChanged (Sha "1b2") "mandatory" Project.BuildSucceeded
            ]
          (finalState, actions) = runActionCustom results $ handleEventsTest events state
        actions `shouldBe`
          [ AIsReviewer (Username "deckard")
          , ALeaveComment (PullRequestId 12) "Pull request approved for merge by @deckard, rebasing now."
          , ATryIntegrate "Merge #12: Twelfth PR\n\n\
                          \Approved-by: deckard\n\
                          \Auto-deploy: false\n"
                          (PullRequestId 12,Branch "refs/pull/12/head",Sha "12a")
                          []
                          False
          , ALeaveComment (PullRequestId 12) "Rebased as 1b2, waiting for CI …"
          , ALeaveComment (PullRequestId 12) "[CI job :yellow_circle:](example.com/required/1b2) started."
          , ATryPromote (Branch "tth") (Sha "1b2")
          , ACleanupTestBranch (PullRequestId 12)
          ]
        -- test caveat, in reality, when Promote works,
        -- the PR is removed from the building list.
        classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
          { building = [PullRequestId 12] -- not here in a real scenario
          , failed   = []
          , approved = []
          , awaiting = []
          }

      context "does not merge when one mandatory check out of many fail irrespective of status arrival" $ do
        let
          projectState = Project.emptyProjectState
            { Project.mandatoryChecks = Project.MandatoryChecks (Set.fromList ["required", "mandatory"]) }
          state
            = Project.insertPullRequest (PullRequestId 12)
                (Branch "tth") masterBranch (Sha "12a") "Twelfth PR"  (Username "person")
            $ projectState
          results = defaultResults {resultIntegrate = [Right (Sha "1b2")]}
          events =
            [ CommentAdded (PullRequestId 12) "deckard" "@bot merge"
            , CommentAdded (PullRequestId 12) "bot" "Pull request approved for merge, rebasing now."
            , CommentAdded (PullRequestId 12) "bot" "Rebased as 1b2, waiting for CI …"
            , BuildStatusChanged (Sha "1b2") "required" (Project.BuildStarted "example.com/required/1b2")
            , BuildStatusChanged (Sha "1b2") "mandatory" (Project.BuildStarted "example.com/mandatory/1b2")
            , CommentAdded (PullRequestId 1) "bot" "[CI job :yellow_circle:](example.com/required/1b2) started."
            ]
          commonAssertions actions finalState = do
            actions `shouldBe`
              [ AIsReviewer (Username "deckard")
              , ALeaveComment (PullRequestId 12) "Pull request approved for merge by @deckard, rebasing now."
              , ATryIntegrate "Merge #12: Twelfth PR\n\n\
                              \Approved-by: deckard\n\
                              \Auto-deploy: false\n"
                              (PullRequestId 12,Branch "refs/pull/12/head",Sha "12a")
                              []
                              False
              , ALeaveComment (PullRequestId 12) "Rebased as 1b2, waiting for CI …"
              , ALeaveComment (PullRequestId 12) "[CI job :yellow_circle:](example.com/required/1b2) started."
              , ALeaveComment (PullRequestId 12) "The build failed :x:.\n\n\
                                                \If this is the result of a flaky test, \
                                                \close and reopen the PR, then tag me again.  \
                                                \Otherwise, push a new commit and tag me again."
              ]
            -- test caveat, in reality, when Promote works,
            -- the PR is removed from the building list.
            classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
              { building = [] -- not here in a real scenario
              , failed   = [PullRequestId 12]
              , approved = []
              , awaiting = []
              }

        it "failure arrives first" $ do
          let
            extraEvents =
              [ BuildStatusChanged (Sha "1b2") "required" (Project.BuildFailed Nothing)
              , BuildStatusChanged (Sha "1b2") "mandatory" Project.BuildSucceeded
              ]
            (finalState, actions) = runActionCustom results $ handleEventsTest (events ++ extraEvents) state
          commonAssertions actions finalState
        it "success arrives first" $ do
          let
            extraEvents =
              [ BuildStatusChanged (Sha "1b2") "mandatory" Project.BuildSucceeded
              , BuildStatusChanged (Sha "1b2") "required" (Project.BuildFailed Nothing)
              ]
            (finalState, actions) = runActionCustom results $ handleEventsTest (events ++ extraEvents) state
          commonAssertions actions finalState
    it "does not unintegrate after failing pull request (not a train)" $ do
      -- regression test for https://github.com/channable/hoff/issues/184
      let
        state
          = Project.insertPullRequest (PullRequestId 19)
              (Branch "fst") masterBranch (Sha "19a") "Nineteenth PR"   (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 36)
              (Branch "snd") masterBranch (Sha "36b") "Thirty-sixth PR" (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 19) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "a19") "default" (Project.BuildFailed Nothing)
          , CommentAdded (PullRequestId 36) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "b36") "default" (Project.BuildFailed Nothing)
          , CommentAdded (PullRequestId 36) "deckard" "@bot merge on Friday"
          , PullRequestClosed (PullRequestId 19)
          , PullRequestClosed (PullRequestId 36)
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "a19")
                                                     , Right (Sha "b36")
                                                     ] }
        (finalState, actions) = runActionCustom results $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 19)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #19: Nineteenth PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 19, Branch "refs/pull/19/head", Sha "19a")
                        []
                        False
        , ALeaveComment (PullRequestId 19) "Rebased as a19, waiting for CI …"
        , ALeaveComment (PullRequestId 19) "The build failed :x:.\n\n\
                                          \If this is the result of a flaky test, \
                                          \close and reopen the PR, then tag me again.  \
                                          \Otherwise, push a new commit and tag me again."
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 36)
                       "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #36: Thirty-sixth PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 36, Branch "refs/pull/36/head", Sha "36b")
                        []
                        False
        , ALeaveComment (PullRequestId 36) "Rebased as b36, waiting for CI …"
        , ALeaveComment (PullRequestId 36) "The build failed :x:.\n\n\
                                          \If this is the result of a flaky test, \
                                          \close and reopen the PR, then tag me again.  \
                                          \Otherwise, push a new commit and tag me again."
        , AIsReviewer (Username "deckard")
        , ALeaveComment (PullRequestId 36) "Your merge request has been denied \
                                          \because it is not Friday. \
                                          \Run merge instead"
        , ACleanupTestBranch (PullRequestId 19)
        , ACleanupTestBranch (PullRequestId 36)
        ]
      classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
        { building = []
        , failed   = []
        , approved = []
        , awaiting = []
        }

    it "correctly updates the metric for the merge train size after each event" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1)
              (Branch "fst") masterBranch (Sha "ab1") "Improvements..." (Username "huey")
          $ Project.insertPullRequest (PullRequestId 2)
              (Branch "snd") masterBranch (Sha "ab2") "... Of the ..." (Username "dewey")
          $ Project.insertPullRequest (PullRequestId 3)
              (Branch "trd") masterBranch (Sha "ab3") "... Performance" (Username "louie")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "ab1") "default" (Project.BuildStarted "url")
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "ab2") "default" (Project.BuildStarted "url")
          , CommentAdded (PullRequestId 3) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "ab3") "default" (Project.BuildStarted "url")

          , BuildStatusChanged (Sha "ab1") "default" Project.BuildSucceeded
          , PullRequestClosed (PullRequestId 1)
          , BuildStatusChanged (Sha "ab2") "default" (Project.BuildFailed Nothing)
          , PullRequestClosed (PullRequestId 2)
          , BuildStatusChanged (Sha "ab3") "default" Project.BuildSucceeded
          , PullRequestClosed (PullRequestId 3)
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "ab1")
                                                     , Right (Sha "ab2")
                                                     , Right (Sha "ab3")
                                                     , Right (Sha "ab1")
                                                     , Right (Sha "ab2")
                                                     , Right (Sha "ab3")
                                                     ] }
        (_, postResults, _) = runActionCustomResults results $ handleEventsTest events state
        -- the gauge is updated after each event, we only care about changes
        stripRepeating = map head . group
      -- note that the updates are stored in reverse
      stripRepeating (resultTrainSizeUpdates postResults) `shouldBe` [0,1,2,3,2,1]

    it "resets the integration of PRs in the train after the PR commit has changed" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.insertPullRequest (PullRequestId 3) (Branch "trd") masterBranch (Sha "ef3") "Third PR"  (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 3) "deckard" "@bot merge"
          , PullRequestCommitChanged (PullRequestId 1) (Sha "4ba")
          ]
        -- For this test, we assume all integrations and pushes succeed.
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2bc")
                                                     , Right (Sha "3cd")
                                                     , Right (Sha "5bc")
                                                     , Right (Sha "6cd") ] }
        (finalState, actions) = runActionCustom results $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2bc behind 1 other PR, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 3)
                        "Pull request approved for merge by @deckard, \
                        \waiting for rebase behind 2 pull requests."
        , ATryIntegrate "Merge #3: Third PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 3, Branch "refs/pull/3/head", Sha "ef3")
                        [PullRequestId 1, PullRequestId 2]
                        False
        , ALeaveComment (PullRequestId 3) "Speculatively rebased as 3cd behind 2 other PRs, waiting for CI …"
        , ALeaveComment (PullRequestId 1) "Stopping integration because the PR changed after approval."
        , ACleanupTestBranch (PullRequestId 1)
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        []
                        False
        , ALeaveComment (PullRequestId 2) "Rebased as 5bc, waiting for CI …"
        , ATryIntegrate "Merge #3: Third PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 3, Branch "refs/pull/3/head", Sha "ef3")
                        [PullRequestId 2]
                        False
        , ALeaveComment (PullRequestId 3) "Speculatively rebased as 6cd behind 1 other PR, waiting for CI …"
        ]
      classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
        { building = [PullRequestId 2, PullRequestId 3]
        , failed   = []
        , approved = []
        , awaiting = [PullRequestId 1]
        }

    it "only notifies rebase failures on top of the master branch (success, rebasefailure, success)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.insertPullRequest (PullRequestId 3) (Branch "trd") masterBranch (Sha "ef3") "Third PR"  (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 3) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildSucceeded)
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Left (IntegrationFailure (BaseBranch "testing/1") RebaseFailed)
                                                     , Right (Sha "3cd") ] }
        (finalState, actions) = runActionCustom results $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        -- We could post a comment like this, but it would be confusing...
        -- , ALeaveComment (PullRequestId 2) "Failed speculative rebase.  Waiting in the queue for a rebase on master."
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 3)
                        "Pull request approved for merge by @deckard, \
                        \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #3: Third PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 3, Branch "refs/pull/3/head", Sha "ef3")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 3) "Speculatively rebased as 3cd behind 1 other PR, waiting for CI …"
        , ATryPromote (Branch "fst") (Sha "1ab")
        , ACleanupTestBranch (PullRequestId 1)
        -- PR#2 is only notified after PR#1 passes or fails
        , ALeaveComment (PullRequestId 2)
                        "Failed to rebase, please rebase manually using\n\n\
                        \    git fetch && git rebase --interactive --autosquash origin/master snd"
        ]
      classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
        { building = [PullRequestId 1, PullRequestId 3]
        , failed   = [PullRequestId 2]
        , approved = []
        , awaiting = []
        }

    it "recovers from speculative rebase failures by starting a new train (failure, rebasefailure, success)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.insertPullRequest (PullRequestId 3) (Branch "trd") masterBranch (Sha "ef3") "Third PR"  (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 3) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildFailed (Just "ci.example.com/1ab"))
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Left (IntegrationFailure (BaseBranch "testing/1") RebaseFailed)
                                                     , Right (Sha "3cd")
                                                     , Right (Sha "5bc")
                                                     , Right (Sha "6cd") ] }
        (finalState, actions) = runActionCustom results $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        -- We could post a comment like this, but it would be confusing...
        -- , ALeaveComment (PullRequestId 2) "Failed speculative rebase.  Waiting in the queue for a rebase on master."
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 3)
                        "Pull request approved for merge by @deckard, \
                        \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #3: Third PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 3, Branch "refs/pull/3/head", Sha "ef3")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 3) "Speculatively rebased as 3cd behind 1 other PR, waiting for CI …"
        , ALeaveComment (PullRequestId 1) "The [build failed :x:](ci.example.com/1ab).\n\n\
                                          \If this is the result of a flaky test, \
                                          \close and reopen the PR, then tag me again.  \
                                          \Otherwise, push a new commit and tag me again."
        -- Since #1 failed, #2 takes over as the head of the new merge train
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        []
                        False
        , ALeaveComment (PullRequestId 2) "Rebased as 5bc, waiting for CI …"
        , ATryIntegrate "Merge #3: Third PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 3, Branch "refs/pull/3/head", Sha "ef3")
                        [PullRequestId 2]
                        False
        , ALeaveComment (PullRequestId 3) "Speculatively rebased as 6cd behind 1 other PR, waiting for CI …"
        ]
      classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
        { building = [PullRequestId 2, PullRequestId 3]
        , failed   = [PullRequestId 1]
        , approved = []
        , awaiting = []
        }

    it "reports wrongfixups regardless of position in train (success, wrongfixups)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Left (IntegrationFailure (BaseBranch "testing/1") WrongFixups)
                                                     , Right (Sha "3cd") ] }
        (finalState, actions) = runActionCustom results $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2)
                        "Pull request cannot be integrated\
                        \ as it contains fixup commits that\
                        \ do not belong to any other commits."
        ]
      classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
        { building = [PullRequestId 1]
        , failed   = [PullRequestId 2]
        , approved = []
        , awaiting = []
        }

    it "new commits on conflicted PRs should not reset other PRs in the train\
       \(success, wrongfixups (success), success)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.insertPullRequest (PullRequestId 3) (Branch "trd") masterBranch (Sha "ef3") "Third PR"  (Username "tyrell")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 3) "deckard" "@bot merge"
          , PullRequestCommitChanged (PullRequestId 2) (Sha "c2d")
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Left (IntegrationFailure (BaseBranch "testing/1") WrongFixups)
                                                     , Right (Sha "3cd")
                                                     , Right (Sha "2bc") ] }
        (finalState, actions) = runActionCustom results $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2)
                        "Pull request cannot be integrated\
                        \ as it contains fixup commits that\
                        \ do not belong to any other commits."
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 3)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #3: Third PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 3, Branch "refs/pull/3/head", Sha "ef3")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 3) "Speculatively rebased as 3cd behind 1 other PR, waiting for CI …"
        -- upon commit changed on PR#2, there is no reason to reintegrate PR#3
        -- PR#2 is moved to the end of the train after a new merge command
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind 2 pull requests."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "c2d")
                        [PullRequestId 1, PullRequestId 3]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2bc behind 2 other PRs, waiting for CI …"
        ]
      classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
        { building = [PullRequestId 1, PullRequestId 3, PullRequestId 2]
        , failed   = []
        , approved = []
        , awaiting = []
        }

    it "handles a 2-wagon merge train with build successes coming in the right order: success (1), success (2)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildSucceeded)
          , BuildStatusChanged (Sha "2cd") "default" (Project.BuildSucceeded)
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2cd") ] }
        run = runActionCustom results
        actions = snd $ run $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2cd behind 1 other PR, waiting for CI …"
        , ATryPromote (Branch "fst") (Sha "1ab")
        , ACleanupTestBranch (PullRequestId 1)
        , ATryPromote (Branch "snd") (Sha "2cd")
        , ACleanupTestBranch (PullRequestId 2)
        ]

    it "handles a 2-wagon merge train with build successes coming in the reverse order: success (2), success (1)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          -- Build of #2 finishes before build of #1
          , BuildStatusChanged (Sha "2cd") "default" (Project.BuildSucceeded)
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildSucceeded)
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2cd") ] }
        run = runActionCustom results
        actions = snd $ run $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2cd behind 1 other PR, waiting for CI …"
        , ATryPromote (Branch "fst") (Sha "1ab")
        , ACleanupTestBranch (PullRequestId 1)
        , ATryPromote (Branch "snd") (Sha "2cd")
        , ACleanupTestBranch (PullRequestId 2)
        ]

    it "handles a 2-wagon merge train with build failures coming in the right order: failure (1), failure (2)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildFailed Nothing)
          , BuildStatusChanged (Sha "2cd") "default" (Project.BuildFailed Nothing)
          , BuildStatusChanged (Sha "22e") "default" (Project.BuildFailed Nothing)
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2cd")
                                                     , Right (Sha "22e") ] }
        (finalState, actions) = runActionCustom results $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2cd behind 1 other PR, waiting for CI …"
        , ALeaveComment (PullRequestId 1) "The build failed :x:.\n\n\
                                          \If this is the result of a flaky test, \
                                          \close and reopen the PR, then tag me again.  \
                                          \Otherwise, push a new commit and tag me again."
        -- #2 is integrated again as its speculative base failed
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        []
                        False
        , ALeaveComment (PullRequestId 2) "Rebased as 22e, waiting for CI …"
        , ALeaveComment (PullRequestId 2) "The build failed :x:.\n\n\
                                          \If this is the result of a flaky test, \
                                          \close and reopen the PR, then tag me again.  \
                                          \Otherwise, push a new commit and tag me again."
        ]
      classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
        { building = []
        , failed   = [PullRequestId 1, PullRequestId 2]
        , approved = []
        , awaiting = []
        }

    it "handles a 2-wagon merge train with build failures coming in the reverse order: failure (2), failure (1)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          -- Build of #2 finishes before build of #1
          , BuildStatusChanged (Sha "2cd") "default" (Project.BuildFailed Nothing)
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildFailed Nothing)
          , BuildStatusChanged (Sha "22e") "default" (Project.BuildFailed Nothing)
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2cd")
                                                     , Right (Sha "22e") ] }
        (finalState, actions) = runActionCustom results $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2cd behind 1 other PR, waiting for CI …"
        , ALeaveComment (PullRequestId 2) "Speculative build failed :x:.  I will automatically retry after getting build results for #1."
        , ALeaveComment (PullRequestId 1) "The build failed :x:.\n\n\
                                          \If this is the result of a flaky test, \
                                          \close and reopen the PR, then tag me again.  \
                                          \Otherwise, push a new commit and tag me again."
        -- #2 is integrated again as its speculative base failed
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        []
                        False
        , ALeaveComment (PullRequestId 2) "Rebased as 22e, waiting for CI …"
        , ALeaveComment (PullRequestId 2) "The build failed :x:.\n\n\
                                          \If this is the result of a flaky test, \
                                          \close and reopen the PR, then tag me again.  \
                                          \Otherwise, push a new commit and tag me again."
        ]
      classifiedPullRequestIds finalState `shouldBe` ClassifiedPullRequestIds
        { building = []
        , failed   = [PullRequestId 1, PullRequestId 2]
        , approved = []
        , awaiting = []
        }

    it "handles a 2-wagon merge train with success and failure coming in the right order: success (1), failure (2)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "1ab") "default" Project.BuildSucceeded
          , BuildStatusChanged (Sha "2cd") "default" (Project.BuildFailed Nothing)
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2cd") ] }
        run = runActionCustom results
        actions = snd $ run $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2cd behind 1 other PR, waiting for CI …"
        , ATryPromote (Branch "fst") (Sha "1ab")
        , ACleanupTestBranch (PullRequestId 1)
        , ALeaveComment (PullRequestId 2) "The build failed :x:.\n\n\
                                          \If this is the result of a flaky test, \
                                          \close and reopen the PR, then tag me again.  \
                                          \Otherwise, push a new commit and tag me again."
        ]

    it "handles a 2-wagon merge train with success and failure coming in the reverse order: success (2), failure (1)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "2cd") "default" (Project.BuildFailed Nothing)
          , BuildStatusChanged (Sha "1ab") "default" Project.BuildSucceeded
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2cd") ] }
        run = runActionCustom results
        actions = snd $ run $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2cd behind 1 other PR, waiting for CI …"
        , ALeaveComment (PullRequestId 2) "Speculative build failed :x:.  I will automatically retry after getting build results for #1."
        , ATryPromote (Branch "fst") (Sha "1ab")
        , ACleanupTestBranch (PullRequestId 1)
        , ALeaveComment (PullRequestId 2) "The build failed :x:.\n\n\
                                          \If this is the result of a flaky test, \
                                          \close and reopen the PR, then tag me again.  \
                                          \Otherwise, push a new commit and tag me again."
        ]

    it "handles a 2-wagon merge train with success and failure coming in the right order: failure (1), success (2)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildFailed Nothing)
          , BuildStatusChanged (Sha "2cd") "default" (Project.BuildFailed Nothing)
          , BuildStatusChanged (Sha "22e") "default" Project.BuildSucceeded
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2cd")
                                                     , Right (Sha "22e") ] }
        run = runActionCustom results
        actions = snd $ run $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2cd behind 1 other PR, waiting for CI …"
        , ALeaveComment (PullRequestId 1) "The build failed :x:.\n\n\
                                          \If this is the result of a flaky test, \
                                          \close and reopen the PR, then tag me again.  \
                                          \Otherwise, push a new commit and tag me again."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        []
                        False
        , ALeaveComment (PullRequestId 2) "Rebased as 22e, waiting for CI …"
        , ATryPromote (Branch "snd") (Sha "22e")
        , ACleanupTestBranch (PullRequestId 2)
        ]

    it "handles a 2-wagon merge train with success and failure coming in the reverse order: failure (2), success (1)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "2cd") "default" Project.BuildSucceeded
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildFailed Nothing)
          , BuildStatusChanged (Sha "22e") "default" Project.BuildSucceeded
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2cd")
                                                     , Right (Sha "22e") ] }
        run = runActionCustom results
        actions = snd $ run $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2cd behind 1 other PR, waiting for CI …"
        , ALeaveComment (PullRequestId 1) "The build failed :x:.\n\n\
                                          \If this is the result of a flaky test, \
                                          \close and reopen the PR, then tag me again.  \
                                          \Otherwise, push a new commit and tag me again."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        []
                        False
        , ALeaveComment (PullRequestId 2) "Rebased as 22e, waiting for CI …"
        , ATryPromote (Branch "snd") (Sha "22e")
        , ACleanupTestBranch (PullRequestId 2)
        ]

    it "handles a 2-wagon merge train with closing and success coming in the reverse order: closing (2), success (1)" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "2cd") "default" Project.BuildSucceeded
          , PullRequestClosed (PullRequestId 1)
          , BuildStatusChanged (Sha "22e") "default" Project.BuildSucceeded
          ]
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2cd")
                                                     , Right (Sha "22e") ] }
        run = runActionCustom results
        actions = snd $ run $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2cd behind 1 other PR, waiting for CI …"
        , ALeaveComment (PullRequestId 1) "Abandoning this pull request because it was closed."
        , ACleanupTestBranch (PullRequestId 1)
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        []
                        False
        , ALeaveComment (PullRequestId 2) "Rebased as 22e, waiting for CI …"
        , ATryPromote (Branch "snd") (Sha "22e")
        , ACleanupTestBranch (PullRequestId 2)
        ]

    it "handles a sequence of merges: success, success, success" $ do
      -- An afternoon of work on PRs:
      -- * three PRs are merged and approved in order
      -- * build always succeeds
      -- * Hoff is notified of its own comments and of GH closing merged PRs
      -- Hoff should process albeit ignore its own comments.
      --
      -- This serves to test and document a complete workflow.
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.insertPullRequest (PullRequestId 3) (Branch "trd") masterBranch (Sha "ef3") "Third PR"  (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ BuildStatusChanged (Sha "ab1") "default" (Project.BuildSucceeded) -- PR#1 sha, ignored
          , CommentAdded (PullRequestId 1) "deckard" "@someone Thanks for your review."
          , CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 1) "bot" "Pull request approved for merge, rebasing now."
          , CommentAdded (PullRequestId 1) "bot" "Rebased as 1ab, waiting for CI …"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "bot" "Pull request approved for merge behind 1 PR."
          , BuildStatusChanged (Sha "ef3") "default" (Project.BuildSucceeded) -- PR#3 sha, ignored
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildPending) -- same status, ignored
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildStarted "example.com/1ab")
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildStarted "example.com/1ab") -- dup!
          , CommentAdded (PullRequestId 1) "bot" "[CI job :yellow_circle:](example.com/1ab) started."
          , CommentAdded (PullRequestId 3) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 3) "bot" "Pull request approved for merge behind 2 PRs."
          , BuildStatusChanged (Sha "cd2") "default" (Project.BuildSucceeded) -- PR#2 sha, ignored
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildSucceeded) -- PR#1
          , PullRequestClosed (PullRequestId 1)
          , CommentAdded (PullRequestId 2) "bot" "Rebased as 2bc, waiting for CI …"
          , BuildStatusChanged (Sha "2bc") "default" (Project.BuildStarted "example.com/2bc")
          , CommentAdded (PullRequestId 2) "bot" "[CI job :yellow_circle:](example.com/2bc) started."
          , BuildStatusChanged (Sha "36a") "default" (Project.BuildSucceeded) -- arbitrary sha, ignored
          , BuildStatusChanged (Sha "2bc") "default" (Project.BuildSucceeded) -- PR#2
          , PullRequestClosed (PullRequestId 2)
          , CommentAdded (PullRequestId 3) "bot" "Rebased as 3cd, waiting for CI …"
          , BuildStatusChanged (Sha "3cd") "default" (Project.BuildStarted "example.com/3cd")
          , BuildStatusChanged (Sha "3cd") "default" (Project.BuildSucceeded) -- PR#3
          , PullRequestClosed (PullRequestId 3)
          ]
        -- For this test, we assume all integrations and pushes succeed.
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2bc")
                                                     , Right (Sha "3cd") ] }
        run = runActionCustom results
        actions = snd $ run $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2bc behind 1 other PR, waiting for CI …"
        , ALeaveComment (PullRequestId 1) "[CI job :yellow_circle:](example.com/1ab) started."
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 3)
                        "Pull request approved for merge by @deckard, \
                        \waiting for rebase behind 2 pull requests."
        , ATryIntegrate "Merge #3: Third PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 3, Branch "refs/pull/3/head", Sha "ef3")
                        [PullRequestId 1, PullRequestId 2]
                        False
        , ALeaveComment (PullRequestId 3) "Speculatively rebased as 3cd behind 2 other PRs, waiting for CI …"
        , ATryPromote (Branch "fst") (Sha "1ab")
        , ACleanupTestBranch (PullRequestId 1)
        , ALeaveComment (PullRequestId 2) "[CI job :yellow_circle:](example.com/2bc) started."
        , ATryPromote (Branch "snd") (Sha "2bc")
        , ACleanupTestBranch (PullRequestId 2)
        , ALeaveComment (PullRequestId 3) "[CI job :yellow_circle:](example.com/3cd) started."
        , ATryPromote (Branch "trd") (Sha "3cd")
        , ACleanupTestBranch (PullRequestId 3)
        ]

    it "handles a sequence of merges: success, failure, success" $ do
      -- An afternoon of work on PRs:
      -- * three PRs are merged and approved in reverse order
      -- * build does not always succeeds
      -- * Hoff is notified of its own comments and of GH auto-closing PRs
      -- Hoff should process albeit ignore its own comments.
      --
      -- This serves to test and document a complete workflow.
      let
        state
          = Project.insertPullRequest (PullRequestId 9) (Branch "nth") masterBranch (Sha "ab9") "Ninth PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 8) (Branch "eth") masterBranch (Sha "cd8") "Eighth PR" (Username "rachael")
          $ Project.insertPullRequest (PullRequestId 7) (Branch "sth") masterBranch (Sha "ef7") "Seventh PR"  (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ BuildStatusChanged (Sha "ab9") "default" (Project.BuildSucceeded) -- PR#9 sha, ignored
          , CommentAdded (PullRequestId 9) "deckard" "@someone Thanks for your review."
          , CommentAdded (PullRequestId 9) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 9) "bot" "Pull request approved for merge, rebasing now."
          , CommentAdded (PullRequestId 9) "bot" "Rebased as 1ab, waiting for CI …"
          , CommentAdded (PullRequestId 8) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 8) "bot" "Pull request approved for merge behind 1 PR."
          , BuildStatusChanged (Sha "ef7") "default" (Project.BuildSucceeded) -- PR#7 sha, ignored
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildPending) -- same status, ignored
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildStarted "example.com/1ab")
          , CommentAdded (PullRequestId 9) "bot" "[CI job :yellow_circle:](example.com/1ab) started."
          , CommentAdded (PullRequestId 7) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 7) "bot" "Pull request approved for merge behind 2 PRs."
          , BuildStatusChanged (Sha "cd8") "default" (Project.BuildSucceeded) -- PR#8 sha, ignored
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildSucceeded) -- PR#9
          , PullRequestClosed (PullRequestId 9)
          , CommentAdded (PullRequestId 8) "bot" "Rebased as 2bc, waiting for CI …"
          , BuildStatusChanged (Sha "2bc") "default" (Project.BuildStarted "example.com/2bc")
          , CommentAdded (PullRequestId 8) "bot" "[CI job :yellow_circle:](example.com/2bc) started."
          , BuildStatusChanged (Sha "3cd") "default" (Project.BuildSucceeded) -- testing build passed on PR#7
          , BuildStatusChanged (Sha "36a") "default" (Project.BuildSucceeded) -- arbitrary sha, ignored
          , BuildStatusChanged (Sha "2bc") "default" (Project.BuildFailed (Just "example.com/2bc")) -- PR#8
          , BuildStatusChanged (Sha "2bc") "default" (Project.BuildFailed (Just "example.com/2bc")) -- dup!
          , CommentAdded (PullRequestId 8) "bot" "The [build failed :x:](example.com/2bc)"
          , CommentAdded (PullRequestId 7) "bot" "Rebased as 3cd, waiting for CI …"
          , BuildStatusChanged (Sha "3ef") "default" (Project.BuildStarted "example.com/3ef")
          , BuildStatusChanged (Sha "3ef") "default" (Project.BuildSucceeded) -- testing build passed on PR#7
          , PullRequestClosed (PullRequestId 7)
          ]
        -- For this test, we assume all integrations and pushes succeed.
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2bc")
                                                     , Right (Sha "3cd")
                                                     , Right (Sha "3ef") ] }
        run = runActionCustom results
        actions = snd $ run $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 9)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #9: Ninth PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 9, Branch "refs/pull/9/head", Sha "ab9")
                        []
                        False
        , ALeaveComment (PullRequestId 9) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 8)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #8: Eighth PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 8, Branch "refs/pull/8/head", Sha "cd8")
                        [PullRequestId 9]
                        False
        , ALeaveComment (PullRequestId 8) "Speculatively rebased as 2bc behind 1 other PR, waiting for CI …"
        , ALeaveComment (PullRequestId 9) "[CI job :yellow_circle:](example.com/1ab) started."
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 7)
                        "Pull request approved for merge by @deckard, \
                        \waiting for rebase behind 2 pull requests."
        , ATryIntegrate "Merge #7: Seventh PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 7, Branch "refs/pull/7/head", Sha "ef7")
                        [PullRequestId 9, PullRequestId 8]
                        False
        , ALeaveComment (PullRequestId 7) "Speculatively rebased as 3cd behind 2 other PRs, waiting for CI …"
        , ATryPromote (Branch "nth") (Sha "1ab")
        , ACleanupTestBranch (PullRequestId 9)
        , ALeaveComment (PullRequestId 8) "[CI job :yellow_circle:](example.com/2bc) started."
        , ALeaveComment (PullRequestId 8)
                        "The [build failed :x:](example.com/2bc).\n\n\
                        \If this is the result of a flaky test, \
                        \close and reopen the PR, then tag me again.  \
                        \Otherwise, push a new commit and tag me again."
        -- Since #8 failed, #7 becomes the head of a new train and is rebased again
        , ATryIntegrate "Merge #7: Seventh PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 7, Branch "refs/pull/7/head", Sha "ef7")
                        []
                        False
        , ALeaveComment (PullRequestId 7) "Rebased as 3ef, waiting for CI …"
        , ALeaveComment (PullRequestId 7) "[CI job :yellow_circle:](example.com/3ef) started."
        , ATryPromote (Branch "sth") (Sha "3ef")
        , ACleanupTestBranch (PullRequestId 7)
        ]

    it "handles a sequence of four successful merges: success, success, success, success" $ do
      -- An afternoon of work on PRs:
      -- * four PRs are merged and approved in order
      -- * build always succeeds
      -- * Hoff is notified of its own comments and of GH closing merged PRs
      -- Hoff should process albeit ignore its own comments.
      --
      -- This serves to test and document a complete workflow.
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "fst") masterBranch (Sha "ab1") "First PR"  (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "snd") masterBranch (Sha "cd2") "Second PR" (Username "rachael")
          $ Project.insertPullRequest (PullRequestId 3) (Branch "trd") masterBranch (Sha "ef3") "Third PR"  (Username "rachael")
          $ Project.insertPullRequest (PullRequestId 4) (Branch "fth") masterBranch (Sha "fe4") "Fourth PR" (Username "rachael")
          $ Project.emptyProjectState
        events =
          [ BuildStatusChanged (Sha "ab1") "default" (Project.BuildSucceeded) -- PR#1 sha, ignored
          , CommentAdded (PullRequestId 1) "deckard" "@someone Thanks for your review."
          , CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 1) "bot" "Pull request approved for merge, rebasing now."
          , CommentAdded (PullRequestId 1) "bot" "Rebased as 1ab, waiting for CI …"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "bot" "Pull request approved for merge behind 1 PR."
          , BuildStatusChanged (Sha "ef3") "default" (Project.BuildSucceeded) -- PR#3 sha, ignored
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildPending) -- same status, ignored
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildStarted "example.com/1ab")
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildStarted "example.com/1ab") -- dup!
          , CommentAdded (PullRequestId 1) "bot" "[CI job :yellow_circle:](example.com/1ab) started."
          , CommentAdded (PullRequestId 3) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 3) "bot" "Pull request approved for merge behind 2 PRs."
          , CommentAdded (PullRequestId 4) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 4) "bot" "Pull request approved for merge behind 3 PRs."
          , BuildStatusChanged (Sha "cd2") "default" (Project.BuildSucceeded) -- PR#2 sha, ignored
          , BuildStatusChanged (Sha "1ab") "default" (Project.BuildSucceeded) -- PR#1
          , PullRequestClosed (PullRequestId 1)
          , CommentAdded (PullRequestId 2) "bot" "Rebased as 2bc, waiting for CI …"
          , BuildStatusChanged (Sha "2bc") "default" (Project.BuildStarted "example.com/2bc")
          , CommentAdded (PullRequestId 2) "bot" "[CI job :yellow_circle:](example.com/2bc) started."
          , BuildStatusChanged (Sha "36a") "default" (Project.BuildSucceeded) -- arbitrary sha, ignored
          , BuildStatusChanged (Sha "2bc") "default" (Project.BuildSucceeded) -- PR#2
          , PullRequestClosed (PullRequestId 2)
          , CommentAdded (PullRequestId 3) "bot" "Rebased as 3cd, waiting for CI …"
          , BuildStatusChanged (Sha "3cd") "default" (Project.BuildStarted "example.com/3cd")
          , BuildStatusChanged (Sha "3cd") "default" (Project.BuildSucceeded) -- PR#3
          , PullRequestClosed (PullRequestId 3)
          , BuildStatusChanged (Sha "4de") "default" (Project.BuildStarted "example.com/4de")
          , BuildStatusChanged (Sha "4de") "default" (Project.BuildSucceeded) -- PR#4
          , PullRequestClosed (PullRequestId 4)
          ]
        -- For this test, we assume all integrations and pushes succeed.
        results = defaultResults { resultIntegrate = [ Right (Sha "1ab")
                                                     , Right (Sha "2bc")
                                                     , Right (Sha "3cd")
                                                     , Right (Sha "4de") ] }
        run = runActionCustom results
        actions = snd $ run $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1)
                        "Pull request approved for merge by @deckard, rebasing now."
        , ATryIntegrate "Merge #1: First PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 1, Branch "refs/pull/1/head", Sha "ab1")
                        []
                        False
        , ALeaveComment (PullRequestId 1) "Rebased as 1ab, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2)
                       "Pull request approved for merge by @deckard, \
                       \waiting for rebase behind one pull request."
        , ATryIntegrate "Merge #2: Second PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 2, Branch "refs/pull/2/head", Sha "cd2")
                        [PullRequestId 1]
                        False
        , ALeaveComment (PullRequestId 2) "Speculatively rebased as 2bc behind 1 other PR, waiting for CI …"
        , ALeaveComment (PullRequestId 1) "[CI job :yellow_circle:](example.com/1ab) started."
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 3)
                        "Pull request approved for merge by @deckard, \
                        \waiting for rebase behind 2 pull requests."
        , ATryIntegrate "Merge #3: Third PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 3, Branch "refs/pull/3/head", Sha "ef3")
                        [PullRequestId 1, PullRequestId 2]
                        False
        , ALeaveComment (PullRequestId 3) "Speculatively rebased as 3cd behind 2 other PRs, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 4)
                        "Pull request approved for merge by @deckard, \
                        \waiting for rebase behind 3 pull requests."
        , ATryIntegrate "Merge #4: Fourth PR\n\n\
                        \Approved-by: deckard\n\
                        \Auto-deploy: false\n"
                        (PullRequestId 4, Branch "refs/pull/4/head", Sha "fe4")
                        [PullRequestId 1, PullRequestId 2, PullRequestId 3]
                        False
        , ALeaveComment (PullRequestId 4) "Speculatively rebased as 4de behind 3 other PRs, waiting for CI …"
        , ATryPromote (Branch "fst") (Sha "1ab")
        , ACleanupTestBranch (PullRequestId 1)
        , ALeaveComment (PullRequestId 2) "[CI job :yellow_circle:](example.com/2bc) started."
        , ATryPromote (Branch "snd") (Sha "2bc")
        , ACleanupTestBranch (PullRequestId 2)
        , ALeaveComment (PullRequestId 3) "[CI job :yellow_circle:](example.com/3cd) started."
        , ATryPromote (Branch "trd") (Sha "3cd")
        , ACleanupTestBranch (PullRequestId 3)
        , ALeaveComment (PullRequestId 4) "[CI job :yellow_circle:](example.com/4de) started."
        , ATryPromote (Branch "fth") (Sha "4de")
        , ACleanupTestBranch (PullRequestId 4)
        ]
