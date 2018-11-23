-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic
(
  Action,
  ActionFree (..),
  Event (..),
  EventQueue,
  dequeueEvent,
  enqueueEvent,
  enqueueStopSignal,
  ensureCloned,
  handleEvent,
  newEventQueue,
  newStateVar,
  readStateVar,
  runAction,
  proceed,
  proceedUntilFixedPoint,
  updateStateVar
)
where

import Control.Concurrent.STM.TMVar (TMVar, newTMVar, readTMVar, swapTMVar)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Control.Monad (when, void)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.STM (atomically)
import Data.Text (Text)
import Data.Text.Format.Params (Params)
import Data.Text.Lazy (toStrict)
import System.FilePath ((</>))

import qualified Data.Text.Format as Text

import Configuration (ProjectConfiguration)
import Git (Branch (..), GitOperation, Sha (..))
import Project (BuildStatus (..), ProjectState, Push (..))

import qualified Git
import qualified Project as Project
import qualified Configuration as Config

-- Conversion function because of Haskell string type madness. This is just
-- Text.format, but returning a strict Text instead of a lazy one.
-- TODO: Extract into utility module and avoid duplication?
format :: Params ps => Text.Format -> ps -> Text
format formatString params = toStrict $ Text.format formatString params

data ActionFree a
  = StartBuild Branch Sha a
  | DeleteBuildDirectory Sha a
  deriving (Functor)

type Action = Free ActionFree

startBuild :: Branch -> Sha -> Action ()
startBuild branch sha = liftF $ StartBuild branch sha ()

_deleteBuildDirectory :: Sha -> Action ()
_deleteBuildDirectory sha = liftF $ DeleteBuildDirectory sha ()

-- Interpreter that translates high-level actions into more low-level ones.
runAction :: ProjectConfiguration -> Action a -> GitOperation a
runAction config action =
  let
    continueWith = runAction config
  in case action of
    Pure result -> pure result
    Free (StartBuild branch sha cont) -> do
      ensureCloned config
      let Sha textSha = sha
      -- Fetch both the exact commit, and the branch, so we have a label
      -- pointing to the branch. There is an opportunity for a race here, when
      -- there are multiple pushes to that branch. TODO: create it locally
      -- instead.
      Git.fetchBranch (Branch textSha)
      -- Name the build directory after the commit: the commit is unique, so if
      -- we can do builds for different commits in parallel.
      -- TODO: Clean them up at some point. This is where DeleteBuildDirectory
      -- would come in.
      let buildDir = (Config.buildDir config) </> (show sha)
      result <- Git.cloneLocal sha branch buildDir
      case result of
        Git.CloneFailed -> pure () -- TODO: Handle errors.
        Git.CloneOk -> do
          -- TODO: Start the build script.
          pure ()
      continueWith cont
    Free (DeleteBuildDirectory _sha cont) -> do
      -- TODO: Actually clean up the build dir.
      continueWith cont

ensureCloned :: ProjectConfiguration -> GitOperation ()
ensureCloned config =
  let
    url = format "git@github.com:{}/{}.git" (Config.owner config, Config.repository config)
    -- Just a very basic retry, no exponential backoff or anything. Also, the
    -- reason that the clone fails might not be a temporary issue, but still;
    -- retrying is the best thing we could do.
    cloneWithRetry 0 = pure ()
    cloneWithRetry (triesLeft :: Int) = do
      result <- Git.clone (Git.RemoteUrl url)
      case result of
        Git.CloneOk -> pure ()
        Git.CloneFailed -> cloneWithRetry (triesLeft - 1)
  in do
    exists <- Git.doesGitDirectoryExist
    when (not exists) (cloneWithRetry 3)
    pure ()

data Event
  = Pushed Push
  | BuildStatusChanged Sha BuildStatus
  deriving (Eq, Show)

type EventQueue = TBQueue (Maybe Event)
type StateVar = TMVar ProjectState

-- Creates a new event queue with the given maximum capacity.
newEventQueue :: Int -> IO EventQueue
newEventQueue capacity = atomically $ newTBQueue capacity

-- Enqueues an event, blocks if the queue is full.
enqueueEvent :: EventQueue -> Event -> IO ()
enqueueEvent queue event = atomically $ writeTBQueue queue $ Just event

-- Signals the event loop to stop after processing all events
-- currently in the queue.
enqueueStopSignal :: EventQueue -> IO ()
enqueueStopSignal queue = atomically $ writeTBQueue queue Nothing

-- Dequeue an event or stop signal from an event queue.
dequeueEvent :: EventQueue -> IO (Maybe Event)
dequeueEvent queue = atomically $ readTBQueue queue

-- Creates a new project state variable.
newStateVar :: ProjectState -> IO StateVar
newStateVar initialState = atomically $ newTMVar initialState

-- Put a new value in the project state variable, discarding the previous one.
updateStateVar :: StateVar -> ProjectState -> IO ()
updateStateVar var state = void $ atomically $ swapTMVar var state

-- Read the most recent value from the project state variable.
readStateVar :: StateVar -> IO ProjectState
readStateVar var = atomically $ readTMVar var

handleEvent :: ProjectConfiguration -> Event -> ProjectState -> ProjectState
handleEvent _config event = case event of
  Pushed push                   -> Project.insertPush push
  BuildStatusChanged sha status -> Project.setBuildStatus sha status

-- Determines if there is anything to do, and if there is, generates the right
-- actions and updates the state accordingly. For example, if there are pending
-- builds, start one.
proceed :: ProjectState -> Action ProjectState
proceed state = case Project.pushesPending state of
  -- No pending pushes, nothing to build.
  [] -> pure state

  (Push sha branch _title _author) : _ -> do
    startBuild branch sha
    pure $ Project.setBuildStatus sha Started state

-- Keep doing a proceed step until the state doesn't change any more. For this
-- to work properly, it is essential that "proceed" does not have any side
-- effects if it does not change the state.
proceedUntilFixedPoint :: ProjectState -> Action ProjectState
proceedUntilFixedPoint state = do
  newState <- proceed state
  if newState == state
    then pure state
    else proceedUntilFixedPoint newState
