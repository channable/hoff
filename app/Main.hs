-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<**>))
import Control.Monad (forM, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.List (zip4)
import System.Exit (die)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

import qualified Control.Concurrent.Async as Async
import qualified Data.Text.Encoding as Text
import qualified GitHub.Auth as Github3
import qualified System.Directory as FileSystem
import qualified Options.Applicative as Opts

import Configuration (Configuration)
import EventLoop (runGithubEventLoop, runLogicEventLoop)
import Project (ProjectState, emptyProjectState, loadProjectState, saveProjectState)
import Project (ProjectInfo (ProjectInfo), Owner)
import Server (buildServer)

import qualified Configuration as Config
import qualified Git
import qualified Github
import qualified GithubApi
import qualified Logic
import qualified Project

data Options = Options
  { configFilePath :: FilePath
  , readOnly :: Bool
  }

commandLineParser :: Opts.ParserInfo Options
commandLineParser =
  let
    optConfigFilePath = Opts.argument Opts.str (Opts.metavar "<config-file>")
    optReadOnly = Opts.switch (Opts.long "read-only")
    opts = Options <$> optConfigFilePath <*> optReadOnly
    help = Opts.fullDesc <> Opts.header "A gatekeeper for your commits"
  in
    Opts.info (opts <**> Opts.helper) help

loadConfigOrExit :: FilePath -> IO Configuration
loadConfigOrExit fname = do
  exists <- FileSystem.doesFileExist fname
  unless exists $
    die $ "Cannot load configuration: the file '" ++ fname ++ "' does not exist."
  maybeConfig <- Config.loadConfiguration fname
  case maybeConfig of
    Right config -> return config
    Left msg -> die $ "Failed to parse configuration file '" ++ fname ++ "'.\n" ++ msg

initializeProjectState :: FilePath -> IO ProjectState
initializeProjectState fname = do
  exists <- FileSystem.doesFileExist fname
  if exists then do
    eitherState <- loadProjectState fname
    case eitherState of
      Right projectState -> do
        putStrLn $ "Loaded project state from '" ++ fname ++ "'."
        return projectState
      Left msg -> do
        -- Fail loudly if something is wrong, and abort the program.
        die $ "Failed to parse project state in '" ++ fname ++ "' with error " ++ msg ++ ".\n" ++
              "Please repair or remove the file."
  else do
    putStrLn $ "File '" ++ fname ++ "' not found, starting with an empty state."
    return emptyProjectState

main :: IO ()
main = Opts.execParser commandLineParser >>= runMain

runMain :: Options -> IO ()
runMain options = do
  -- When the runtime detects that stdout is not connected to a console, it
  -- defaults to block buffering instead of line buffering. When running under
  -- systemd, this prevents log messages (which are written to stdout) from
  -- showing up until the buffer is flushed. Therefore, explicitly select line
  -- buffering, to enforce a flush after every newline.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  putStrLn $ "Config file: " ++ (configFilePath options)
  putStrLn $ "Read-only: " ++ (show $ readOnly options)

  -- Load configuration from the file specified as first program argument.
  config <- loadConfigOrExit $ configFilePath options

  -- Create an event queue for GitHub webhook events. The server enqueues events
  -- here when a webhook is received, and a worker thread will process these
  -- events. Limit the number of queued events to 10 to avoid overloading the
  -- server: new hooks are rejected when the queue is full. Webhooks are
  -- low-volume (in the range of ~once per minute) and processing events
  -- should be fast (a few milliseconds, or perhaps a few seconds for a heavy
  -- Git operation), so the queue is expected to be empty most of the time.
  ghQueue <- Github.newEventQueue 10

  -- Events do not stay in the webhook queue for long: they are converted into
  -- logic events and put in the project queues, where the main event loop will
  -- process them. This conversion process does not reject events, but it blocks
  -- if the project queue is full (which will cause the webhook queue to fill
  -- up, so the server will reject new events).
  projectQueues <- forM (Config.projects config) $ \ pconfig -> do
    projectQueue <- Logic.newEventQueue 10
    let
      owner        = Config.owner pconfig
      repository   = Config.repository pconfig
      projectInfo  = ProjectInfo owner repository
    return (projectInfo, projectQueue)

  -- Define a function that enqueues an event in the right project queue.
  let
    enqueueEvent projectInfo event =
      -- Call the corresponding enqueue function if the project exists,
      -- otherwise drop the event on the floor.
      maybe (return ()) (\ queue -> Logic.enqueueEvent queue event) $
      -- TODO: This is doing a linear scan over a linked list to find the right
      -- queue. That can be improved. A lot.
      lookup projectInfo projectQueues

  -- Start a worker thread to put the GitHub webhook events in the right queue.
  ghThread <- Async.async $ runStdoutLoggingT $ runGithubEventLoop ghQueue enqueueEvent

  -- Restore the previous state from disk if possible, or start clean.
  projectStates <- forM (Config.projects config) $ \ pconfig -> do
    projectState <- initializeProjectState (Config.stateFile pconfig)
    let
      -- TODO: DRY.
      owner        = Config.owner pconfig
      repository   = Config.repository pconfig
      projectInfo  = ProjectInfo owner repository
    return (projectInfo, projectState)

  -- Keep track of the most recent state for every project, so the webinterface
  -- can use it to serve a status page.
  stateVars <- forM projectStates $ \ (projectInfo, projectState) -> do
    stateVar <- Logic.newStateVar projectState
    return (projectInfo, stateVar)

  -- Start a main event loop for every project.
  let
    -- TODO: This is very, very ugly. Get these per-project collections sorted
    -- out.
    zipped = zip4 (Config.projects config) projectQueues stateVars projectStates
    tuples = map (\(cfg, (_, a), (_, b), (_, c)) -> (cfg, a, b, c)) zipped
  projectThreads <- forM tuples $ \ (projectConfig, projectQueue, stateVar, projectState) -> do
    -- At startup, enqueue a synchronize event. This will bring the state in
    -- sync with the current state of GitHub, accounting for any webhooks that
    -- we missed while not running, or just to fill the state initially after
    -- setting up a new project.
    liftIO $ Logic.enqueueEvent projectQueue Logic.Synchronize

    let
      -- When the event loop publishes the current project state, save it to
      -- the configured file, and make the new state available to the
      -- webinterface.
      publish newState = do
        liftIO $ saveProjectState (Config.stateFile projectConfig) newState
        liftIO $ Logic.updateStateVar stateVar newState

      -- When the event loop wants to get the next event, take one off the queue.
      getNextEvent = liftIO $ Logic.dequeueEvent projectQueue

      -- In the production app, we interpret both Git actions and GitHub actions
      -- to the real thing in IO. In the tests, when calling `runLogicEventLoop`
      -- we could swap one or both of them out for a test implementation.
      repoDir     = Config.checkout projectConfig
      auth        = Github3.OAuth $ Text.encodeUtf8 $ Config.accessToken config
      projectInfo = ProjectInfo (Config.owner projectConfig) (Config.repository projectConfig)
      runGit = if readOnly options
        then Git.runGitReadOnly (Config.user config) repoDir
        else Git.runGit         (Config.user config) repoDir
      runGithub = if readOnly options
        then GithubApi.runGithubReadOnly auth projectInfo
        else GithubApi.runGithub         auth projectInfo

    -- Start a worker thread to run the main event loop for the project.
    Async.async
      $ void
      $ runStdoutLoggingT
      $ runLogicEventLoop
          (Config.trigger config)
          projectConfig
          runGit
          runGithub
          getNextEvent
          publish
          projectState

  let
    -- When the webhook server receives an event, enqueue it on the webhook
    -- event queue if it is not full.
    ghTryEnqueue = Github.tryEnqueueEvent ghQueue

    -- Allow the webinterface to retrieve the latest project state per project.
    getProjectState projectInfo =
      fmap Logic.readStateVar $ lookup projectInfo stateVars
    getOwnerState :: Owner -> IO [(ProjectInfo, ProjectState)]
    getOwnerState owner = do
      let states = filter (\(projectInfo, _) -> Project.owner projectInfo == owner) stateVars
      mapM (\(info, state) ->  Logic.readStateVar state >>= \sVar -> pure (info, sVar)) states

  let
    port      = Config.port config
    tlsConfig = Config.tls config
    secret    = Config.secret config
    -- TODO: Do this in a cleaner way.
    infos     = fmap (\ pc -> ProjectInfo (Config.owner pc) (Config.repository pc)) $ Config.projects config
  putStrLn $ "Listening for webhooks on port " ++ (show port) ++ "."
  runServer <- fmap fst $ buildServer port tlsConfig infos secret ghTryEnqueue getProjectState getOwnerState
  serverThread <- Async.async runServer

  -- Note that a stop signal is never enqueued. The application just runs until
  -- until it is killed, or until any of the threads stop due to an exception.
  void $ Async.waitAny $ serverThread : ghThread : projectThreads
