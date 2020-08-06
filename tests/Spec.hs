-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Free (foldFree)
import Control.Monad.Trans.RWS.Strict (RWS)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (readFile)
import Data.Foldable (foldlM)
import Data.IntSet (IntSet)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (Text)
import Prelude hiding (readFile)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec

import qualified Control.Monad.Trans.RWS.Strict as Rws
import qualified Data.Text as Text
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.UUID.V4 as Uuid

import EventLoop (convertGithubEvent)
import Git (Branch (..), PushResult(..), Sha (..), parsePorcelainPushOutput)
import Github (CommentPayload, CommitStatusPayload, PullRequestPayload)
import Logic (Action, ActionFree (..), Event (..), IntegrationFailure (..))
import Project (ProjectState (ProjectState), PullRequest (PullRequest))
import Types (PullRequestId (..), Username (..))

import qualified Configuration as Config
import qualified Github
import qualified GithubApi
import qualified Logic
import qualified Project

-- Trigger config used throughout these tests.
testTriggerConfig :: Config.TriggerConfiguration
testTriggerConfig = Config.TriggerConfiguration {
  Config.commentPrefix = "@bot"
}

-- Functions to prepare certain test states.

singlePullRequestState :: PullRequestId -> Branch -> Sha -> Username -> ProjectState
singlePullRequestState pr prBranch prSha prAuthor =
  let
    event = PullRequestOpened pr prBranch prSha "Untitled" prAuthor
  in
    fst $ runAction $ handleEventTest event Project.emptyProjectState

candidateState :: PullRequestId -> Branch -> Sha -> Username -> Username -> Sha -> ProjectState
candidateState pr prBranch prSha prAuthor approver candidateSha =
  let
    state = Project.setIntegrationStatus pr
      (Project.Integrated candidateSha Project.BuildPending)
      $ Project.setApproval pr (Just approver)
      $ singlePullRequestState pr prBranch prSha prAuthor
  in
    state { Project.integrationCandidate = Just pr }

-- Types and functions to mock running an action without actually doing anything.

data ActionFlat
  = ATryIntegrate Text (Branch, Sha)
  | ATryPromote Branch Sha
  | ALeaveComment PullRequestId Text
  | AIsReviewer Username
  | AGetPullRequest PullRequestId
  | AGetOpenPullRequests
  deriving (Eq, Show)

-- Results to return from various operations during the tests. There is a
-- default, but specific tests can override some results, to test failure cases.
data Results = Results
  { resultIntegrate :: [Either IntegrationFailure Sha]
  , resultPush :: [PushResult]
  , resultGetPullRequest :: [Maybe GithubApi.PullRequest]
  , resultGetOpenPullRequests :: [Maybe IntSet]
  }

defaultResults :: Results
defaultResults = Results
    -- Pretend that integration always conflicts.
  { resultIntegrate = repeat $ Left $ Logic.IntegrationFailure $ Branch "master"
    -- Pretend that pushing is always successful.
  , resultPush = repeat $ PushOk
    -- Pretend that these two calls to GitHub always fail.
  , resultGetPullRequest = repeat $ Nothing
  , resultGetOpenPullRequests = repeat $ Nothing
  }

-- Consume the head of the field with given getter and setter in the Results.
takeFromList
  :: HasCallStack
  => Monoid w
  => String
  -> (Results -> [a])
  -> ([a] -> Results -> Results)
  -> RWS r w Results a
takeFromList name getField setField = do
  values <- Rws.gets getField
  Rws.modify $ setField $ tail values
  case values of
    []  -> error $ "Not enough results supplied for " <> name <> "."
    v:_ -> pure v

takeResultIntegrate :: (HasCallStack, Monoid w) => RWS r w Results (Either IntegrationFailure Sha)
takeResultIntegrate =
  takeFromList
    "resultIntegrate"
    resultIntegrate
    (\v res -> res { resultIntegrate = v })

takeResultPush :: (HasCallStack, Monoid w) => RWS r w Results PushResult
takeResultPush =
  takeFromList
    "resultPush"
    resultPush
    (\v res -> res { resultPush = v })

takeResultGetPullRequest :: (HasCallStack, Monoid w) => RWS r w Results (Maybe GithubApi.PullRequest)
takeResultGetPullRequest =
  takeFromList
    "resultGetPullRequest"
    resultGetPullRequest
    (\v res -> res { resultGetPullRequest = v })

takeResultGetOpenPullRequests :: (HasCallStack, Monoid w) => RWS r w Results (Maybe IntSet)
takeResultGetOpenPullRequests =
  takeFromList
    "resultGetOpenPullRequests"
    resultGetOpenPullRequests
    (\v res -> res { resultGetOpenPullRequests = v })

-- This function simulates running the actions, and returns the final state,
-- together with a list of all actions that would have been performed. Some
-- actions require input from the outside world. Simulating these actions will
-- consume one entry from the `Results` in the state.
runActionRws :: HasCallStack => Action a -> RWS () [ActionFlat] Results a
runActionRws =
  let
    -- In the tests, only "deckard" is a reviewer.
    isReviewer username = username == "deckard"
  in
    foldFree $ \case
      TryIntegrate msg candidate cont -> do
        Rws.tell [ATryIntegrate msg candidate]
        cont <$> takeResultIntegrate
      TryPromote prBranch headSha cont -> do
        Rws.tell [ATryPromote prBranch headSha]
        cont <$> takeResultPush
      LeaveComment pr body cont -> do
        Rws.tell [ALeaveComment pr body]
        pure cont
      IsReviewer username cont -> do
        Rws.tell [AIsReviewer username]
        pure $ cont $ isReviewer username
      GetPullRequest pr cont -> do
        Rws.tell [AGetPullRequest pr]
        cont <$> takeResultGetPullRequest
      GetOpenPullRequests cont -> do
        Rws.tell [AGetOpenPullRequests]
        cont <$> takeResultGetOpenPullRequests

-- Simulates running the action. Use the provided results as result for various
-- operations. Results are consumed one by one.
runActionCustom :: HasCallStack => Results -> Action a -> (a, [ActionFlat])
runActionCustom results action = Rws.evalRWS (runActionRws action) () results

-- Simulates running the action with default results.
runAction :: Action a -> (a, [ActionFlat])
runAction = runActionCustom defaultResults

-- Handle an event, then advance the state until a fixed point,
-- and simulate its side effects.
handleEventTest :: Event -> ProjectState -> Action ProjectState
handleEventTest event state = Logic.handleEvent testTriggerConfig event state

-- Handle events (advancing the state until a fixed point in between) and
-- simulate their side effects.
handleEventsTest :: [Event] -> ProjectState -> Action ProjectState
handleEventsTest events state = foldlM (flip $ Logic.handleEvent testTriggerConfig) state events

main :: IO ()
main = hspec $ do
  describe "Logic.handleEvent" $ do

    it "handles PullRequestOpened" $ do
      let event = PullRequestOpened (PullRequestId 3) (Branch "p") (Sha "e0f") "title" "lisa"
          state = fst $ runAction $ handleEventTest event Project.emptyProjectState
      state `shouldSatisfy` Project.existsPullRequest (PullRequestId 3)
      let pr = fromJust $ Project.lookupPullRequest (PullRequestId 3) state
      Project.sha pr               `shouldBe` Sha "e0f"
      Project.author pr            `shouldBe` "lisa"
      Project.approvedBy pr        `shouldBe` Nothing
      Project.integrationStatus pr `shouldBe` Project.NotIntegrated

    it "handles PullRequestClosed" $ do
      let event1 = PullRequestOpened (PullRequestId 1) (Branch "p") (Sha "abc") "title" "peter"
          event2 = PullRequestOpened (PullRequestId 2) (Branch "q") (Sha "def") "title" "jack"
          event3 = PullRequestClosed (PullRequestId 1)
          state  = fst $ runAction $ handleEventsTest [event1, event2, event3] Project.emptyProjectState
      state `shouldSatisfy` not . Project.existsPullRequest (PullRequestId 1)
      state `shouldSatisfy` Project.existsPullRequest (PullRequestId 2)

    it "handles closing the integration candidate PR" $ do
      let event  = PullRequestClosed (PullRequestId 1)
          state  = candidateState (PullRequestId 1) (Branch "p") (Sha "ea0") "frank" "deckard" (Sha "cf4")
          state' = fst $ runAction $ handleEventTest event state
      Project.integrationCandidate state' `shouldBe` Nothing

    it "does not modify the integration candidate if a different PR was closed" $ do
      let event  = PullRequestClosed (PullRequestId 1)
          state  = candidateState (PullRequestId 2) (Branch "p") (Sha "a38") "franz" "deckard" (Sha "ed0")
          state' = fst $ runAction $ handleEventTest event state
      Project.integrationCandidate state' `shouldBe` (Just $ PullRequestId 2)

    it "loses approval after the PR commit has changed" $ do
      let event  = PullRequestCommitChanged (PullRequestId 1) (Sha "def")
          state0 = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "abc") "alice"
          state1 = Project.setApproval (PullRequestId 1) (Just "hatter") state0
          state2 = fst $ runAction $ handleEventTest event state1
          pr1    = fromJust $ Project.lookupPullRequest (PullRequestId 1) state1
          pr2    = fromJust $ Project.lookupPullRequest (PullRequestId 1) state2
      Project.approvedBy pr1 `shouldBe` Just "hatter"
      Project.approvedBy pr2 `shouldBe` Nothing

    it "does not lose approval after the PR commit has changed due to a push we caused" $ do
      let
        state0 = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "abc") "alice"
        state1 = Project.setApproval (PullRequestId 1) (Just "hatter") state0
        state2 = Project.setIntegrationStatus (PullRequestId 1) (Project.Integrated (Sha "dc0") Project.BuildPending) state1
        state3 = Project.setIntegrationStatus (PullRequestId 1) (Project.Integrated (Sha "dc1") Project.BuildPending) state2
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
        state1  = candidateState (PullRequestId 1) (Branch "p") (Sha "abc") "thomas" "deckard" (Sha "bcd")
        newPush = PullRequestCommitChanged (PullRequestId 1) (Sha "def")
        state2  = fst $ runAction $ handleEventTest newPush state1
        prAt1   = fromJust $ Project.lookupPullRequest (PullRequestId 1) state1
        prAt2   = fromJust $ Project.lookupPullRequest (PullRequestId 1) state2
      Project.approvedBy        prAt1 `shouldBe` Just "deckard"
      Project.integrationStatus prAt1 `shouldBe` Project.Integrated (Sha "bcd") Project.BuildPending
      Project.approvedBy        prAt2 `shouldBe` Nothing
      Project.integrationStatus prAt2 `shouldBe` Project.NotIntegrated

    it "ignores false positive commit changed events" $ do
      let
        -- This commit change is not really a change, therefore we should not
        -- lose the approval status.
        event  = PullRequestCommitChanged (PullRequestId 1) (Sha "000")
        state0 = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "000") "cindy"
        state1 = Project.setApproval (PullRequestId 1) (Just "daniel") state0
        (state2, _actions) = runAction $ Logic.proceedUntilFixedPoint state1
        (state3, actions)  = runAction $ handleEventTest event state2
        prAt3 = fromJust $ Project.lookupPullRequest (PullRequestId 1) state3
      state3 `shouldBe` state2
      actions `shouldBe` []
      Project.approvedBy prAt3 `shouldBe` Just "daniel"

    it "sets approval after a stamp from a reviewer" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "6412ef5") "toby"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approvedBy pr `shouldBe` Just "deckard"

    it "does not set approval after a stamp from a non-reviewer" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "6412ef5") "toby"
          -- Note: the comment is a valid approval command, but "rachael" is not
          -- marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "rachael" "@bot merge"
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approvedBy pr `shouldBe` Nothing

    it "does not set approval after a comment with the wrong prefix" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "6412ef5") "patrick"
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
      Project.approvedBy pr `shouldBe` Nothing

    it "accepts command comments case-insensitively" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "6412ef5") "sacha"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "@BoT MeRgE"
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approvedBy pr `shouldBe` Just "deckard"

    it "accepts command at end of other comments" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "6412ef5") "sacha"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "looks good to me, @bot merge"
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approvedBy pr `shouldBe` Just "deckard"

    it "accepts command at end of other comments if tagged multiple times" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "6412ef5") "sacha"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "@bot looks good to me, @bot merge"
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approvedBy pr `shouldBe` Just "deckard"

    it "accepts command before comments" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "6412ef5") "sacha"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "@bot merge\nYou did some fine work here."
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approvedBy pr `shouldBe` Just "deckard"

    it "does not accepts merge command with interleaved comments" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "6412ef5") "sacha"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "@bot foo merge"
          state' = fst $ runAction $ handleEventTest event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approvedBy pr `shouldBe` Nothing

    it "handles a build status change of the integration candidate" $ do
      let
        event  = BuildStatusChanged (Sha "84c") Project.BuildSucceeded
        state  = candidateState (PullRequestId 1) (Branch "p") (Sha "a38") "johanna" "deckard" (Sha "84c")
        state' = fst $ runAction $ handleEventTest event state
        pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.integrationStatus pr `shouldBe` Project.Integrated (Sha "84c") Project.BuildSucceeded

    it "ignores a build status change for commits that are not the integration candidate" $ do
      let
        event0 = PullRequestOpened (PullRequestId 2) (Branch "p") (Sha "0ad") "title" "harry"
        event1 = BuildStatusChanged (Sha "0ad") Project.BuildSucceeded
        state  = candidateState (PullRequestId 1) (Branch "p") (Sha "a38") "harry" "deckard" (Sha "84c")
        state' = fst $ runAction $ handleEventsTest [event0, event1] state
        pr1    = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
        pr2    = fromJust $ Project.lookupPullRequest (PullRequestId 2) state'
      -- Even though the build status changed for "0ad" which is a known commit,
      -- only the build status of the integration candidate can be changed.
      Project.integrationStatus pr1 `shouldBe` Project.Integrated (Sha "84c") Project.BuildPending
      Project.integrationStatus pr2 `shouldBe` Project.NotIntegrated

    it "only checks if a comment author is a reviewer for comment commands" $ do
      let
        state = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "a38") "tyrell"
        event0 = CommentAdded (PullRequestId 1) "deckard" "I don't get it, Tyrell"
        event1 = CommentAdded (PullRequestId 1) "deckard" "@bot merge"
        actions0 = snd $ runAction $ handleEventTest event0 state
        actions1 = snd $ runAction $ handleEventTest event1 state
      actions0 `shouldBe` []
      actions1 `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved by @deckard, rebasing now."
        , ATryIntegrate "Merge #1\n\nApproved-by: deckard" (Branch "refs/pull/1/head", Sha "a38")
        , ALeaveComment (PullRequestId 1)
            "Failed to rebase, please rebase manually using\n\n\
            \    git rebase --interactive --autosquash origin/master p"
        ]

    it "notifies approvers about queue position" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "p") (Sha "a38") "Add Nexus 7 experiment" (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "s") (Sha "dec") "Some PR" (Username "rachael")
          $ Project.insertPullRequest (PullRequestId 3) (Branch "s") (Sha "f16") "Another PR" (Username "rachael")
          $ Project.emptyProjectState
        -- Approve pull request in order of ascending id.
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 3) "deckard" "@bot merge"
          ]
        -- For this test, we assume all integrations and pushes succeed.
        results = defaultResults { resultIntegrate = [Right (Sha "b71")] }
        run = runActionCustom results
        actions = snd $ run $ handleEventsTest events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved by @deckard, rebasing now."
        , ATryIntegrate "Merge #1\n\nApproved-by: deckard" (Branch "refs/pull/1/head", Sha "a38")
        , ALeaveComment (PullRequestId 1) "Rebased as b71, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2) "Pull request approved by @deckard, waiting for rebase at the front of the queue."
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 3) "Pull request approved by @deckard, waiting for rebase behind 2 pull requests."
        ]

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
        , ALeaveComment (PullRequestId 2) "Pull request approved by @deckard, rebasing now."
        , ATryIntegrate "Merge #2\n\nApproved-by: deckard" (Branch "refs/pull/2/head", Sha "dec")
        , ALeaveComment (PullRequestId 2) "Rebased as b71, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved by @deckard, waiting for rebase at the front of the queue."
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 3) "Pull request approved by @deckard, waiting for rebase behind 2 pull requests."
        ]

    it "abandons integration when a pull request is closed" $ do
      let
        state
          = Project.insertPullRequest (PullRequestId 1) (Branch "p") (Sha "a38") "Add Nexus 7 experiment" (Username "tyrell")
          $ Project.insertPullRequest (PullRequestId 2) (Branch "s") (Sha "dec") "Some PR" (Username "rachael")
          $ Project.emptyProjectState
        -- Approve both pull requests, then close the first.
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
          , PullRequestClosed (PullRequestId 1)
          ]
        -- For this test, we assume all integrations and pushes succeed.
        results = defaultResults
          { resultIntegrate = [Right (Sha "b71"), Right (Sha "b72")]
          }
        run = runActionCustom results
        (state', actions) = run $ handleEventsTest events state

      -- The first pull request should be dropped, and a comment should be
      -- left indicating why. Then the second pull request should be at the
      -- front of the queue.
      Project.integrationCandidate state' `shouldBe` Just (PullRequestId 2)
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved by @deckard, rebasing now."
        , ATryIntegrate "Merge #1\n\nApproved-by: deckard" (Branch "refs/pull/1/head", Sha "a38")
        , ALeaveComment (PullRequestId 1) "Rebased as b71, waiting for CI …"
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2) "Pull request approved by @deckard, waiting for rebase at the front of the queue."
        , ALeaveComment (PullRequestId 1) "Abandoning this pull request because it was closed."
        , ATryIntegrate "Merge #2\n\nApproved-by: deckard" (Branch "refs/pull/2/head", Sha "dec")
        , ALeaveComment (PullRequestId 2) "Rebased as b72, waiting for CI …"
        ]

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
        state = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "b7332ba") "tyrell"
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
        state = singlePullRequestState (PullRequestId 10) (Branch "p") (Sha "b7332ba") "tyrell"
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
        state = singlePullRequestState (PullRequestId 19) (Branch "p") (Sha "b7332ba") "tyrell"
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
              { GithubApi.sha    = Sha "7faa52318"
              , GithubApi.branch = Branch "nexus-7"
              , GithubApi.title  = "Add Nexus 7 experiment"
              , GithubApi.author = Username "tyrell"
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
      Project.approvedBy pr17          `shouldBe` Nothing
      Project.integrationStatus pr17   `shouldBe` Project.NotIntegrated
      Project.integrationAttempts pr17 `shouldBe` []
      actions `shouldBe` [AGetOpenPullRequests, AGetPullRequest (PullRequestId 17)]

    it "does not query details of existing pull requests on synchronize" $ do
      let
        state = singlePullRequestState (PullRequestId 19) (Branch "p") (Sha "b7332ba") "tyrell"
        results = defaultResults
          { resultGetOpenPullRequests = [Just $ IntSet.singleton 19]
          }
        actions = snd $ runActionCustom results $ handleEventTest Synchronize state

      -- We should only obtain pull request details for pull requests that were
      -- missing. In this case, PR 19 was already present, so we should not have
      -- obtained its details.
      actions `shouldBe` [AGetOpenPullRequests]

  describe "Logic.proceedUntilFixedPoint" $ do

    it "finds a new candidate" $ do
      let
        state
          = Project.setApproval (PullRequestId 1) (Just "fred")
          $ singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "f34") "sally"
        results = defaultResults
          { resultIntegrate = [Right (Sha "38c")]
          , resultPush = [ PushRejected  (Text.pack "Push output parsing failed") (Text.pack "No reason specified")]
          }
        (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
        (prId, pullRequest) = fromJust $ Project.getIntegrationCandidate state'
      Project.integrationStatus pullRequest `shouldBe` Project.Integrated (Sha "38c") Project.BuildPending
      prId    `shouldBe` PullRequestId 1
      actions `shouldBe`
        [ ATryIntegrate "Merge #1\n\nApproved-by: fred" (Branch "refs/pull/1/head", Sha "f34")
        , ALeaveComment (PullRequestId 1) "Rebased as 38c, waiting for CI \x2026"
        ]

    it "pushes after a successful build" $ do
      let
        pullRequest = PullRequest
          { Project.branch              = Branch "results/rachael"
          , Project.sha                 = Sha "f35"
          , Project.title               = "Add my test results"
          , Project.author              = "rachael"
          , Project.approvedBy          = Just "deckard"
          , Project.integrationStatus   = Project.Integrated (Sha "38d") Project.BuildSucceeded
          , Project.integrationAttempts = []
          , Project.needsFeedback       = False
          }
        state = ProjectState
          { Project.pullRequests         = IntMap.singleton 1 pullRequest
          , Project.integrationCandidate = Just $ PullRequestId 1
          }
        results = defaultResults { resultIntegrate = [Right (Sha "38e")] }
        (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
        candidate = Project.getIntegrationCandidate state'
      -- After a successful push, the candidate should be gone.
      candidate `shouldBe` Nothing
      actions   `shouldBe` [ATryPromote (Branch "results/rachael") (Sha "38d")]

    it "restarts the sequence after a rejected push" $ do
      -- Set up a pull request that has gone through the review and build cycle,
      -- and is ready to be pushed to master.
      let
        pullRequest = PullRequest
          { Project.branch              = Branch "results/rachael"
          , Project.sha                 = Sha "f35"
          , Project.title               = "Add my test results"
          , Project.author              = "rachael"
          , Project.approvedBy          = Just "deckard"
          , Project.integrationStatus   = Project.Integrated (Sha "38d") Project.BuildSucceeded
          , Project.integrationAttempts = []
          , Project.needsFeedback       = False
          }
        state = ProjectState
          { Project.pullRequests         = IntMap.singleton 1 pullRequest
          , Project.integrationCandidate = Just $ PullRequestId 1
          }
        -- Run 'proceedUntilFixedPoint', and pretend that pushes fail (because
        -- something was pushed in the mean time, for instance).
        results = defaultResults
          { resultIntegrate = [Right (Sha "38e")]
          , resultPush = [ PushRejected  (Text.pack "Push output parsing failed") (Text.pack "No reason specified")]
          }
        (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
        (_, pullRequest') = fromJust $ Project.getIntegrationCandidate state'

      Project.integrationStatus   pullRequest' `shouldBe` Project.Integrated (Sha "38e") Project.BuildPending
      Project.integrationAttempts pullRequest' `shouldBe` [Sha "38d"]
      actions `shouldBe`
        [ ATryPromote (Branch "results/rachael") (Sha "38d")
        , ATryIntegrate "Merge #1\n\nApproved-by: deckard" (Branch "refs/pull/1/head", Sha "f35")
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
              (Sha "a39")
              "Add Nexus 7 experiment"
              (Username "tyrell")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "b71") Project.BuildPending
          , BuildStatusChanged (Sha "b71") Project.BuildSucceeded
          ]
        -- For this test, the first integration succeeds. Then we push, which
        -- fails. Then we try to integrate again, but that fails.
        results = defaultResults
          { resultIntegrate =
            [ Right $ Sha "b71"
            , Left $ Logic.IntegrationFailure (Branch "master")
            ]
          , resultPush = [ PushRejected  (Text.pack "Push output parsing failed") (Text.pack "No reason specified")]
          }
        (_state', actions) = runActionCustom results $ handleEventsTest events state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved by @deckard, rebasing now."
        , ATryIntegrate "Merge #1\n\nApproved-by: deckard" (Branch "refs/pull/1/head", Sha "a39")
          -- The first rebase succeeds.
        , ALeaveComment (PullRequestId 1) "Rebased as b71, waiting for CI \x2026"
          -- The first promotion attempt fails
        , ATryPromote (Branch "n7") (Sha "b71")
          -- The second rebase fails.
        , ATryIntegrate "Merge #1\n\nApproved-by: deckard" (Branch "refs/pull/1/head", Sha "a39")
        , ALeaveComment (PullRequestId 1)
            "Failed to rebase, please rebase manually using\n\n\
            \    git rebase --interactive --autosquash origin/master n7"
        ]

    it "picks a new candidate from the queue after a successful push" $ do
      let pullRequest1 = PullRequest
            {
              Project.branch              = Branch "results/leon",
              Project.sha                 = Sha "f35",
              Project.title               = "Add Leon test results",
              Project.author              = "rachael",
              Project.approvedBy          = Just "deckard",
              Project.integrationStatus   = Project.Integrated (Sha "38d") Project.BuildSucceeded,
              Project.integrationAttempts = [],
              Project.needsFeedback       = False
            }
          pullRequest2 = PullRequest
            {
              Project.branch              = Branch "results/rachael",
              Project.sha                 = Sha "f37",
              Project.title               = "Add my test results",
              Project.author              = "rachael",
              Project.approvedBy          = Just "deckard",
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
              Project.integrationCandidate = Nothing
            }
          -- Proceeding should pick the next pull request as candidate.
          results = defaultResults { resultIntegrate = [Right (Sha "38e")] }
          (state', actions) = runActionCustom results $ Logic.proceedUntilFixedPoint state
          Just (cId, _candidate) = Project.getIntegrationCandidate state'
      cId     `shouldBe` PullRequestId 2
      actions `shouldBe`
        [ ATryIntegrate "Merge #2\n\nApproved-by: deckard" (Branch "refs/pull/2/head", Sha "f37")
        , ALeaveComment (PullRequestId 2) "Rebased as 38e, waiting for CI \x2026"
        ]

    it "reports the build status if a user retries the same commit" $ do
      let
        state
          = Project.insertPullRequest
              (PullRequestId 1)
              (Branch "n7")
              (Sha "a39")
              "Add Nexus 7 experiment"
              (Username "tyrell")
          $ Project.emptyProjectState
        events =
          [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          , BuildStatusChanged (Sha "b71") Project.BuildPending
          , BuildStatusChanged (Sha "b71") Project.BuildFailed
            -- User summons bot again because CI failed for an external reason.
          , CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          ]
        results = defaultResults { resultIntegrate = [Right (Sha "b71")] }
        (_state', actions) = runActionCustom results $ handleEventsTest events state

      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved by @deckard, rebasing now."
        , ATryIntegrate "Merge #1\n\nApproved-by: deckard" (Branch "refs/pull/1/head", Sha "a39")
        , ALeaveComment (PullRequestId 1) "Rebased as b71, waiting for CI \x2026"
        , ALeaveComment (PullRequestId 1) "The build failed."
        , AIsReviewer "deckard"
          -- Nothing has changed for the bot because b71 has already failed, so
          -- it doesn't retry, but reports the correct state.
        , ALeaveComment (PullRequestId 1) "The build failed."
        ]

  describe "Github._Payload" $ do

    it "parses a PullRequestPayload correctly" $ do
      examplePayload <- readFile "tests/data/pull-request-payload.json"
      let maybePayload :: Maybe PullRequestPayload
          maybePayload = decode examplePayload
      maybePayload `shouldSatisfy` isJust
      let payload    = fromJust maybePayload
          action     = Github.action     (payload :: PullRequestPayload)
          owner      = Github.owner      (payload :: PullRequestPayload)
          repository = Github.repository (payload :: PullRequestPayload)
          number     = Github.number     (payload :: PullRequestPayload)
          headSha    = Github.sha        (payload :: PullRequestPayload)
          title      = Github.title      (payload :: PullRequestPayload)
          prAuthor   = Github.author     (payload :: PullRequestPayload)
          prBranch   = Github.branch     (payload :: PullRequestPayload)
      action     `shouldBe` Github.Opened
      owner      `shouldBe` "baxterthehacker"
      repository `shouldBe` "public-repo"
      number     `shouldBe` 1
      headSha    `shouldBe` (Sha "0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c")
      prBranch   `shouldBe` (Branch "changes")
      title      `shouldBe` "Update the README with new information"
      prAuthor   `shouldBe` "baxterthehacker2"

    it "parses a CommentPayload correctly" $ do
      examplePayload <- readFile "tests/data/issue-comment-payload.json"
      let maybePayload :: Maybe CommentPayload
          maybePayload = decode examplePayload
      maybePayload `shouldSatisfy` isJust
      let payload       = fromJust maybePayload
          action        = Github.action     (payload :: CommentPayload)
          owner         = Github.owner      (payload :: CommentPayload)
          repository    = Github.repository (payload :: CommentPayload)
          number        = Github.number     (payload :: CommentPayload)
          commentAuthor = Github.author     (payload :: CommentPayload)
          commentBody   = Github.body       (payload :: CommentPayload)
      action        `shouldBe` Github.Created
      owner         `shouldBe` "baxterthehacker"
      repository    `shouldBe` "public-repo"
      number        `shouldBe` 2
      commentAuthor `shouldBe` "baxterthehacker2"
      commentBody   `shouldBe` "You are totally right! I'll get this fixed right away."

    it "parses a CommitStatusPayload correctly" $ do
      examplePayload <- readFile "tests/data/status-payload.json"
      let maybePayload :: Maybe CommitStatusPayload
          maybePayload = decode examplePayload
      maybePayload `shouldSatisfy` isJust
      let payload       = fromJust maybePayload
          owner         = Github.owner      (payload :: CommitStatusPayload)
          repository    = Github.repository (payload :: CommitStatusPayload)
          status        = Github.status     (payload :: CommitStatusPayload)
          url           = Github.url        (payload :: CommitStatusPayload)
          commitSha     = Github.sha        (payload :: CommitStatusPayload)
      owner      `shouldBe` "baxterthehacker"
      repository `shouldBe` "public-repo"
      status     `shouldBe` Github.Success
      url        `shouldBe` Nothing
      commitSha  `shouldBe` (Sha "9049f1265b7d61be4a8904a9a27120d2064dab3b")

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

      Config.owner      project `shouldBe` "your-github-username-or-organization"
      Config.repository project `shouldBe` "your-repo"
      Config.branch     project `shouldBe` "master"
      Config.testBranch project `shouldBe` "testing"
      Config.checkout   project `shouldBe` "/var/lib/hoff/checkouts/your-username/your-repo"
      Config.stateFile  project `shouldBe` "/var/lib/hoff/state/your-username/your-repo.json"

      Config.name user          `shouldBe` "CI Bot"
      Config.email user         `shouldBe` "cibot@example.com"
      Config.sshConfigFile user `shouldBe` "/etc/hoff/ssh_config"

      Config.commentPrefix trigger `shouldBe` "@hoffbot"

  describe "EventLoop.convertGithubEvent" $ do

    let testPullRequestPayload action = Github.PullRequestPayload
          { action     = action
          , owner      = "deckard"
          , repository = "repo"
          , number     = 1
          , branch     = Branch "results"
          , sha        = Sha "b26354"
          , title      = "Add test results"
          , author     = "rachael"
          }

    it "converts a pull request opened event" $ do
      let payload = testPullRequestPayload Github.Opened
          Just event = convertGithubEvent $ Github.PullRequest payload
      event `shouldBe`
        (PullRequestOpened (PullRequestId 1) (Branch "results") (Sha "b26354") "Add test results" "rachael")

    it "converts a pull request reopened event" $ do
      let payload = testPullRequestPayload Github.Reopened
          Just event = convertGithubEvent $ Github.PullRequest payload
      -- Reopened is treated just like opened, there is no memory in the system.
      event `shouldBe`
        (PullRequestOpened (PullRequestId 1) (Branch "results") (Sha "b26354") "Add test results" "rachael")

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
      let payload = testCommentPayload Github.Created
          Just event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` (CommentAdded (PullRequestId 1) "deckard" "Must be expensive.")

    it "ignores a comment edited event" $ do
      let payload = testCommentPayload Github.Edited
          event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` Nothing

    it "ignores a comment deleted event" $ do
      let payload = testCommentPayload Github.Deleted
          event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` Nothing

    let testCommitStatusPayload status = Github.CommitStatusPayload
          { owner      = "rachael"
          , repository = "owl"
          , status     = status
          , url        = Just "https://travis-ci.org/rachael/owl/builds/1982"
          , sha        = Sha "b26354"
          }

    it "converts a commit status pending event" $ do
      let payload = testCommitStatusPayload Github.Pending
          Just event = convertGithubEvent $ Github.CommitStatus payload
      event `shouldBe` (BuildStatusChanged (Sha "b26354") Project.BuildPending)

    it "converts a commit status success event" $ do
      let payload = testCommitStatusPayload Github.Success
          Just event = convertGithubEvent $ Github.CommitStatus payload
      event `shouldBe` (BuildStatusChanged (Sha "b26354") Project.BuildSucceeded)

    it "converts a commit status failure event" $ do
      let payload = testCommitStatusPayload Github.Failure
          Just event = convertGithubEvent $ Github.CommitStatus payload
      event `shouldBe` (BuildStatusChanged (Sha "b26354") Project.BuildFailed)

    it "converts a commit status error event" $ do
      let payload = testCommitStatusPayload Github.Error
          Just event = convertGithubEvent $ Github.CommitStatus payload
      -- The error and failure statuses are both converted to "failed".
      event `shouldBe` (BuildStatusChanged (Sha "b26354") Project.BuildFailed)

  describe "ProjectState" $ do

    it "can be restored exactly after roundtripping through json" $ do
      let
        emptyState  = Project.emptyProjectState
        singleState = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "071") "deckard"
        candState   = candidateState (PullRequestId 2) (Branch "p") (Sha "073") "rachael" "tyrell" (Sha "079")
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
          state = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "071") "deckard"
      Project.saveProjectState fname state
      Just state' <- Project.loadProjectState fname
      state `shouldBe` state'
      removeFile fname

  describe "Git.parsePorcelainPushOutput" $ do
    it "can parse succesfull push results (force push output)" $ do
      let raw = Text.pack "To github.com:foo/bar.git\n+\trefs/heads/master:refs/heads/master\t33b4e8a...bf59dbb (forced update)\nDone\n"
          actual = parsePorcelainPushOutput raw
          expected = Just (Text.pack "33b4e8a...bf59dbb ", Just $ Text.pack "(forced update)")
      actual `shouldBe` expected

    it "can parse failed push results (file to big)" $ do
      let raw = Text.pack "To github.com:foo/bar.git\n!\trefs/heads/master:refs/heads/master\t[remote rejected] (pre-receive hook declined)\n"
          actual = parsePorcelainPushOutput raw
          expected = Just (Text.pack "[remote rejected] ", Just $ Text.pack "(pre-receive hook declined)")
      actual `shouldBe` expected

    it "can parse results when no reason is presest" $ do
      let raw = Text.pack "To github.com:foo/bar.git\n=\trefs/heads/foobar:refs/heads/foobar\t[up to date]\nDone\n"
          actual = parsePorcelainPushOutput raw
          expected = Just (Text.pack "[up to date]", Nothing)
      actual `shouldBe` expected

    it "can parse when newlines are missing (by returning Nothing)" $ do
      let raw = Text.pack "To github.com:foo/bar.git=\trefs/heads/foobar:refs/heads/foobar\t[up to date]Done"
          actual = parsePorcelainPushOutput raw
          expected = Nothing
      actual `shouldBe` expected

    it "can parse when tabs are missing (by returning Nothing)" $ do
      let raw = Text.pack "To github.com:foo/bar.git\n=trefs/heads/foobar:refs/heads/foobar[up to date]\nDone"
          actual = parsePorcelainPushOutput raw
          expected = Nothing
      actual `shouldBe` expected

    it "can handle gibberish input (by returning Nothing)" $ do
      let raw = Text.pack "askjdlfhaosdyuro3\t\t\t\n\n\n\t\t\tasasdkjaslkdjalksjd"
          actual = parsePorcelainPushOutput raw
          expected = Nothing
      actual `shouldBe` expected