-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
import Control.Monad.Free (foldFree)
import Data.Functor.Sum (Sum (InL, InR))

import qualified Data.Text as Text

import Configuration (ProjectConfiguration, TriggerConfiguration)
import Github (PullRequestPayload, CommentPayload, CommitStatusPayload, WebhookEvent (..))
import Github (eventProjectInfo)
import Project (ProjectInfo (..), ProjectState, PullRequestId (..))

import qualified Git
import qualified Github
import qualified GithubApi
import qualified Logic
import qualified Project

eventFromPullRequestPayload :: PullRequestPayload -> Logic.Event
eventFromPullRequestPayload payload =
  let
    number = Github.number (payload :: PullRequestPayload) -- TODO: Use PullRequestId wrapper from beginning.
    title  = Github.title  (payload :: PullRequestPayload)
    author = Github.author (payload :: PullRequestPayload)
    branch = Github.branch (payload :: PullRequestPayload)
    sha    = Github.sha    (payload :: PullRequestPayload)
    base   = Github.base   (payload :: PullRequestPayload)
  in
    case Github.action (payload :: PullRequestPayload) of
      Github.Opened      -> Logic.PullRequestOpened (PullRequestId number) branch sha base title author
      Github.Reopened    -> Logic.PullRequestOpened (PullRequestId number) branch sha base title author
      Github.Closed      -> Logic.PullRequestClosed (PullRequestId number)
      -- TODO: Also deal with title updates.
      Github.Synchronize -> Logic.PullRequestCommitChanged (PullRequestId number) sha

eventFromCommentPayload :: CommentPayload -> Maybe Logic.Event
eventFromCommentPayload payload =
  let number = Github.number (payload :: CommentPayload) -- TODO: Use PullRequestId wrapper from beginning.
      author = Github.author (payload :: CommentPayload) -- TODO: Wrapper type
      body   = Github.body   (payload :: CommentPayload)
  in case Github.action (payload :: CommentPayload) of
    Github.Created -> Just $ Logic.CommentAdded (PullRequestId number) author body
    -- Do not bother with edited and deleted comments, as it would tremendously
    -- complicate handling of approval. Once approved, this cannot be undone.
    -- And if approval undo is desired, it would be better implemented as a
    -- separate magic comment, rather than editing the approval comment.
    Github.Edited  -> Nothing
    Github.Deleted -> Nothing

mapCommitStatus :: Github.CommitStatus -> Project.BuildStatus
mapCommitStatus status = case status of
  Github.Pending -> Project.BuildPending
  Github.Success -> Project.BuildSucceeded
  Github.Failure -> Project.BuildFailed
  Github.Error   -> Project.BuildFailed

eventFromCommitStatusPayload :: CommitStatusPayload -> Logic.Event
eventFromCommitStatusPayload payload =
  let sha    = Github.sha    (payload :: CommitStatusPayload)
      status = Github.status (payload :: CommitStatusPayload)
  in  Logic.BuildStatusChanged sha (mapCommitStatus status)

convertGithubEvent :: Github.WebhookEvent -> Maybe Logic.Event
convertGithubEvent event = case event of
  Ping                 -> Nothing -- TODO: What to do with this one?
  PullRequest payload  -> Just $ eventFromPullRequestPayload payload
  CommitStatus payload -> Just $ eventFromCommitStatusPayload payload
  Comment payload      -> eventFromCommentPayload payload

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
      logDebugN $ Text.append "github loop received event: " (Text.pack $ show ghEvent)
      when (shouldHandle ghEvent) $
        -- If conversion yielded an event, enqueue it. Block if the queue is full.
        let
          projectInfo = eventProjectInfo ghEvent
          converted   = convertGithubEvent ghEvent
        in
          maybe (return ()) (liftIO . enqueueEvent projectInfo) converted
      runLoop

runSum
  :: Monad m
  => (forall a. f a -> m a)
  -> (forall a. g a -> m a)
  -> (forall a. (Sum f g) a -> m a)
runSum runF runG = go
  where
    go (InL u) = runF u
    go (InR v) = runG v

runLogicEventLoop
  :: MonadIO m
  => MonadLogger m
  => TriggerConfiguration
  -> ProjectConfiguration
  -- Interpreters for Git and GitHub actions.
  -> (forall a. Git.GitOperationFree a -> m a)
  -> (forall a. GithubApi.GithubOperationFree a -> m a)
  -- Action that gets the next event from the queue.
  -> m (Maybe Logic.Event)
  -- Action to perform after the state has changed, such as
  -- persisting the new state, and making it available to the
  -- webinterface.
  -> (ProjectState -> m ())
  -> ProjectState
  -> m ProjectState
runLogicEventLoop triggerConfig projectConfig runGit runGithub getNextEvent publish initialState =
  let
    runAll      = foldFree (runSum runGit runGithub)
    runAction   = Logic.runAction projectConfig

    handleAndContinue state0 event = do
      -- Handle the event and then perform any additional required actions until
      -- the state reaches a fixed point (when there are no further actions to
      -- perform).
      logInfoN  $ Text.append "logic loop received event: " (Text.pack $ show event)
      logDebugN $ Text.append "state before: " (Text.pack $ show state0)
      state1 <- runAll $ runAction $ Logic.handleEvent triggerConfig event state0
      publish state1
      logDebugN $ Text.append "state after: " (Text.pack $ show state1)
      runLoop state1

    runLoop state = do
      -- Before anything, clone the repository if there is no clone.
      foldFree runGit $ Logic.ensureCloned projectConfig
      -- Take one event off the queue, block if there is none.
      eventOrStopSignal <- getNextEvent
      -- Queue items are of type 'Maybe Event'; 'Nothing' signals loop
      -- termination. If there was an event, run one iteration and recurse.
      case eventOrStopSignal of
        Just event -> handleAndContinue state event
        Nothing    -> return state

  in
    runLoop initialState
