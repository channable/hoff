-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module EventLoop
(
  convertGithubEvent, -- An internal helper function, but exposed for testing.
  runGithubEventLoop,
  runLogicEventLoop
)
where

import Control.Concurrent.STM.TBQueue
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logInfoN)
import Control.Monad.STM (atomically)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Effectful (Eff, (:>), IOE)
import qualified Data.Text as Text

import Configuration (ProjectConfiguration, TriggerConfiguration, MergeWindowExemptionConfiguration, FeatureFreezeWindow)
import Github (PullRequestPayload, CommentPayload, CommitStatusPayload, PushPayload, WebhookEvent (..))
import Github (eventProjectInfo)
import MonadLoggerEffect (MonadLoggerEffect)
import Project (ProjectInfo (..), ProjectState, PullRequestId (..))
import Time ( TimeOperation )

import qualified Configuration as Config
import qualified Git
import qualified Github
import qualified GithubApi
import qualified Logic
import qualified Project
import qualified Metrics.Metrics as Metrics

eventFromPullRequestPayload :: PullRequestPayload -> Logic.Event
eventFromPullRequestPayload payload =
  let
    number = PullRequestId payload.number
    title  = payload.title
    author = payload.author
    body   = payload.body
    branch = payload.branch
    sha    = payload.sha
    baseBranch = Github.baseBranch (payload :: PullRequestPayload)
  in
    case payload.action of
      Github.Opened      -> Logic.PullRequestOpened number branch baseBranch sha title author body
      -- If the pull request is reopened, we do not want to parse the command from
      -- the body, because it means that a person manually intervened with the pull
      -- request. This is why we do not pass the body to the event, so we don't
      -- accidentally repeatedly process the same command.
      Github.Reopened    -> Logic.PullRequestOpened number branch baseBranch sha title author Nothing
      Github.Closed      -> Logic.PullRequestClosed number
      Github.Synchronize -> Logic.PullRequestCommitChanged number sha
      Github.Edited      -> Logic.PullRequestEdited number title baseBranch

eventFromCommentPayload :: CommentPayload -> Maybe Logic.Event
eventFromCommentPayload payload =
  let number = PullRequestId payload.number
      author = payload.author -- TODO: Wrapper type
      body   = payload.body
      commentAdded = Logic.CommentAdded number author body
  in case payload.action of
    Left Github.CommentCreated -> Just commentAdded
    Right Github.ReviewSubmitted -> Just commentAdded
    -- Do not bother with edited and deleted comments, as it would tremendously
    -- complicate handling of approval. Once approved, this cannot be undone.
    -- And if approval undo is desired, it would be better implemented as a
    -- separate magic comment, rather than editing the approval comment.
    _ -> Nothing

mapCommitStatus :: Github.CommitStatus -> Maybe Text.Text -> Project.BuildStatus
mapCommitStatus status murl = case status of
  Github.Pending -> case murl of
                    Nothing -> Project.BuildPending
                    Just url -> Project.BuildStarted url
  Github.Success -> Project.BuildSucceeded
  Github.Failure -> Project.BuildFailed murl
  Github.Error   -> Project.BuildFailed murl

eventFromCommitStatusPayload :: CommitStatusPayload -> Logic.Event
eventFromCommitStatusPayload payload =
  let sha     = payload.sha
      status  = payload.status
      url     = payload.url
      context = payload.context
  in  Logic.BuildStatusChanged sha context (mapCommitStatus status url)

eventFromPushPayload :: PushPayload -> Logic.Event
eventFromPushPayload payload = Logic.PushPerformed payload.branch payload.sha

convertGithubEvent :: Github.WebhookEvent -> Maybe Logic.Event
convertGithubEvent event = case event of
  Ping                 -> Nothing -- TODO: What to do with this one?
  PullRequest payload  -> Just $ eventFromPullRequestPayload payload
  CommitStatus payload -> Just $ eventFromCommitStatusPayload payload
  Comment payload      -> eventFromCommentPayload payload
  Push payload         -> Just $ eventFromPushPayload payload

-- The event loop that converts GitHub webhook events into logic events.
runGithubEventLoop
  :: (MonadIO m, MonadLogger m)
  => Github.EventQueue
  -> (ProjectInfo -> Logic.Event -> IO ()) -> m ()
runGithubEventLoop ghQueue enqueueEvent = runLoop
  where
    shouldHandle ghEvent = (ghEvent /= Ping)
    runLoop = do
      ghEvent <- liftIO $ atomically $ readTBQueue ghQueue
      let projectInfo = eventProjectInfo ghEvent
      logDebugN $ "github loop received event: " <> showText ghEvent
      when (shouldHandle ghEvent) $
        -- If conversion yielded an event, enqueue it. Block if the queue is full.
        traverse_ (liftIO . enqueueEvent projectInfo) (convertGithubEvent ghEvent)
      runLoop

runLogicEventLoop
  ::  forall es
  .  IOE :> es
  => Metrics.MetricsOperation :> es
  => Time.TimeOperation :> es
  => Git.GitOperation :> es
  => GithubApi.GithubOperation :> es
  => MonadLoggerEffect :> es
  => TriggerConfiguration
  -> ProjectConfiguration
  -> MergeWindowExemptionConfiguration
  -> Maybe FeatureFreezeWindow
  -- Action that gets the next event from the queue.
  -> IO (Maybe Logic.Event)
  -- Action to perform after the state has changed, such as
  -- persisting the new state, and making it available to the
  -- webinterface.
  -> (ProjectState -> IO ())
  -> ProjectState
  -> Eff es ProjectState
runLogicEventLoop
  triggerConfig projectConfig mergeWindowExemptionConfig featureFreezeWindow
  getNextEvent publish initialState =
  let
    repo             = Config.repository projectConfig
    handleAndContinue state0 event = do
      -- Handle the event and then perform any additional required actions until
      -- the state reaches a fixed point (when there are no further actions to
      -- perform).
      logInfoN  $ "logic loop received event (" <> repo <> "): " <> showText event
      state1 <-
        Logic.handleEvent triggerConfig mergeWindowExemptionConfig featureFreezeWindow event state0
      liftIO $ publish state1
      runLoop state1

    runLoop state = do
      -- Before anything, clone the repository if there is no clone.
      Logic.ensureCloned projectConfig
      -- Take one event off the queue, block if there is none.
      eventOrStopSignal <- liftIO getNextEvent
      -- Queue items are of type 'Maybe Event'; 'Nothing' signals loop
      -- termination. If there was an event, run one iteration and recurse.
      case eventOrStopSignal of
        Just event -> handleAndContinue state event
        Nothing    -> return state

  in do
    Logic.runAction projectConfig $
      Logic.runRetrieveEnvironment projectConfig $
      runLoop initialState

showText :: Show a => a -> Text
showText  =  Text.pack . show
