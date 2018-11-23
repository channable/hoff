-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Configuration (ProjectConfiguration, UserConfiguration)
import Github (PushPayload, CommitStatusPayload, WebhookEvent (..))
import Github (eventProjectInfo)
import Project (ProjectInfo, ProjectState)

import qualified Configuration as Config
import qualified Data.Text as Text
import qualified Git
import qualified Github
import qualified Logic
import qualified Project

mapCommitStatus :: Github.CommitStatus -> Project.BuildStatus
mapCommitStatus status = case status of
  Github.Pending -> Project.Pending
  Github.Success -> Project.Succeeded
  Github.Failure -> Project.Failed
  Github.Error   -> Project.Failed

eventFromCommitStatusPayload :: CommitStatusPayload -> Logic.Event
eventFromCommitStatusPayload payload =
  let
    sha    = Github.sha    (payload :: CommitStatusPayload)
    status = Github.status (payload :: CommitStatusPayload)
  in
    Logic.BuildStatusChanged sha (mapCommitStatus status)

eventFromPushPayload :: PushPayload -> Maybe Logic.Event
eventFromPushPayload payload = case Github.commits payload of
  [] -> Nothing
  commit : _ ->
    let
      branch  = Github.branch (payload :: PushPayload)
      sha     = Github.sha    (payload :: PushPayload)
      message = Github.message commit
      author  = Github.authorName commit
    in
      Just $ Logic.Pushed $ Project.Push sha branch message author

convertGithubEvent :: Github.WebhookEvent -> Maybe Logic.Event
convertGithubEvent event = case event of
  Ping                 -> Nothing
  Push payload         -> eventFromPushPayload payload
  CommitStatus payload -> Just $ eventFromCommitStatusPayload payload

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

runLogicEventLoop
  :: (MonadIO m, MonadLogger m)
  => UserConfiguration
  -> ProjectConfiguration
  -- Action that gets the next event from the queue.
  -> m (Maybe Logic.Event)
  -- Action to perform after the state has changed, such as
  -- persisting the new state, and making it available to the
  -- webinterface.
  -> (ProjectState -> m ())
  -> ProjectState
  -> m ProjectState
runLogicEventLoop userConfig projectConfig getNextEvent publish initialState =
  let
    repoDir = Config.checkout projectConfig
    runGit = Git.runGit userConfig repoDir
    runAction = Logic.runAction projectConfig
    handleAndContinue state0 event = do
      -- Handle the event and then perform any additional required actions until
      -- the state reaches a fixed point (when there are no further actions to
      -- perform).
      logInfoN  $ Text.append "logic loop received event: " (Text.pack $ show event)
      logDebugN $ Text.append "state before: " (Text.pack $ show state0)
      state1 <- runGit
                $ runAction
                $ Logic.proceedUntilFixedPoint
                $ Logic.handleEvent projectConfig event state0
      publish state1
      logDebugN $ Text.append "state after: " (Text.pack $ show state1)
      runLoop state1
    runLoop state = do
      -- Before anything, clone the repository if there is no clone.
      runGit $ Logic.ensureCloned projectConfig
      -- Take one event off the queue, block if there is none.
      eventOrStopSignal <- getNextEvent
      -- Queue items are of type 'Maybe Event'; 'Nothing' signals loop
      -- termination. If there was an event, run one iteration and recurse.
      case eventOrStopSignal of
        Just event -> handleAndContinue state event
        Nothing    -> return state
  in
    runLoop initialState
