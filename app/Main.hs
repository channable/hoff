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
import Data.Maybe (fromMaybe, maybeToList)
import Data.String (fromString)
import Data.Version (showVersion)
import Effectful (runEff)
import GHC.Natural (Natural)
import System.Exit (die)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

import Control.Concurrent.Async qualified as Async
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as Text
import GitHub.Auth qualified as Github3
import Options.Applicative qualified as Opts
import System.Directory qualified as FileSystem

import ClockTickLoop (clockTickLoop)
import Configuration (ClockTickInterval (..), Configuration, MetricsConfiguration (metricsHost, metricsPort))
import EventLoop (runGithubEventLoop, runLogicEventLoop)
import MonadLoggerEffect (runLoggerStdout)
import Project (
  Owner,
  ProjectInfo (ProjectInfo),
  ProjectState,
  emptyProjectState,
  loadProjectState,
  saveProjectState,
  subMapByOwner,
 )
import Server (buildServer)

import Metrics.Metrics qualified as Metrics
import Metrics.Server (
  MetricsServerConfig (
    MetricsServerConfig,
    metricsConfigHost,
    metricsConfigPort
  ),
  runMetricsServer,
 )

import Paths_hoff qualified (version)

import Configuration qualified as Config
import Data.Set qualified as Set
import Data.Text qualified as Text
import Git qualified
import Github qualified
import GithubApi qualified
import Logic qualified
import Project qualified
import Time qualified

version :: String
version = showVersion Paths_hoff.version

-- | The number of webhook events Hoff can handle at once. This is shared
-- between projects.
webhookQueueSize :: Natural
webhookQueueSize = 128

-- | The number of queued up events for a single project. This does not need to
-- be the same size as 'webhookQueueSize', but if we run the risk of filling up
-- the webhook queue because of a burst of webhook events then there's a decent
-- chance they're all for the same project.
projectQueueSize :: Natural
projectQueueSize = 128

data Options = Options
  { configFilePath :: FilePath
  , readOnly :: Bool
  }

commandLineParser :: Opts.ParserInfo Options
commandLineParser =
  let
    optConfigFilePath = Opts.argument Opts.str (Opts.metavar "<config-file>")
    optReadOnly =
      Opts.switch $
        Opts.long "read-only"
          <> Opts.help "Run in read-only mode"
    optVersion =
      Opts.infoOption ("Hoff v" <> version) $
        Opts.long "version"
          <> Opts.help "Displays version and exit"
    opts = Options <$> optConfigFilePath <*> optReadOnly <* optVersion
    help = Opts.fullDesc <> Opts.header "A gatekeeper for your commits"
  in
    Opts.info (opts <**> Opts.helper) help

loadConfigOrExit :: FilePath -> IO Configuration
loadConfigOrExit fname = do
  exists <- FileSystem.doesFileExist fname
  unless exists $
    die $
      "Cannot load configuration: the file '" ++ fname ++ "' does not exist."
  maybeConfig <- Config.loadConfiguration fname
  case maybeConfig of
    Right config -> return config
    Left msg -> die $ "Failed to parse configuration file '" ++ fname ++ "'.\n" ++ msg

-- | Initialize an empty project state.
-- Note that the mandatory checks for a project are always fetched from the
-- configuration file.
initializeProjectState :: Project.MandatoryChecks -> FilePath -> IO ProjectState
initializeProjectState mandatory fname = do
  exists <- FileSystem.doesFileExist fname
  let setMandatoryChecks state = state{Project.mandatoryChecks = mandatory}
  if exists
    then do
      eitherState <- loadProjectState fname
      case eitherState of
        Right projectState -> do
          putStrLn $ "Loaded project state from '" ++ fname ++ "'."
          return (setMandatoryChecks projectState)
        Left msg -> do
          -- Fail loudly if something is wrong, and abort the program.
          die $
            "Failed to parse project state in '"
              ++ fname
              ++ "' with error "
              ++ msg
              ++ ".\n"
              ++ "Please repair or remove the file."
    else do
      putStrLn $ "File '" ++ fname ++ "' not found, starting with an empty state."
      return (setMandatoryChecks emptyProjectState)

main :: IO ()
main = Opts.execParser commandLineParser >>= runMain

getProjectInfo :: Config.ProjectConfiguration -> ProjectInfo
getProjectInfo pconfig = ProjectInfo owner repository
 where
  owner = Config.owner pconfig
  repository = Config.repository pconfig

runMain :: Options -> IO ()
runMain options = do
  -- When the runtime detects that stdout is not connected to a console, it
  -- defaults to block buffering instead of line buffering. When running under
  -- systemd, this prevents log messages (which are written to stdout) from
  -- showing up until the buffer is flushed. Therefore, explicitly select line
  -- buffering, to enforce a flush after every newline.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  putStrLn $ "Starting Hoff v" ++ version
  putStrLn $ "Config file: " ++ configFilePath options
  putStrLn $ "Read-only: " ++ show (readOnly options)

  -- Load configuration from the file specified as first program argument.
  config <- loadConfigOrExit $ configFilePath options

  -- Create an event queue for GitHub webhook events. The server enqueues events
  -- here when a webhook is received, and a worker thread will process these
  -- events. Limit the number of queued events to 10 to avoid overloading the
  -- server: new hooks are rejected when the queue is full. Webhooks are
  -- low-volume (in the range of ~once per minute) and processing events
  -- should be fast (a few milliseconds, or perhaps a few seconds for a heavy
  -- Git operation), so the queue is expected to be empty most of the time.
  ghQueue <- Github.newEventQueue webhookQueueSize

  -- Each project is treated in isolation, containing their own queue and state.
  let mandatoryChecksFromConfig projectConfig =
        Project.MandatoryChecks $
          Set.mapMonotonic Project.Check $
            maybe mempty Config.mandatory $
              Config.checks projectConfig
  projectThreadState <-
    Map.fromList
      <$> forM
        (Config.projects config)
        ( \pconfig -> do
            -- Events do not stay in the webhook queue for long: they are converted into
            -- logic events and put in the project queues, where the main event loop will
            -- process them. This conversion process does not reject events, but it blocks
            -- if the project queue is full (which will cause the webhook queue to fill
            -- up, so the server will reject new events).
            projectQueue <- Logic.newEventQueue projectQueueSize

            -- Restore the previous state from disk if possible, or start clean.
            projectState <-
              initializeProjectState
                (mandatoryChecksFromConfig pconfig)
                (Config.stateFile pconfig)

            -- Keep track of the most recent state for every project, so the webinterface
            -- can use it to serve a status page.
            stateVar <- Logic.newStateVar projectState
            let initializationState = ProjectThreadData pconfig projectQueue stateVar
            return (getProjectInfo pconfig, initializationState)
        )

  -- Define a function that enqueues an event in the right project queue.
  let enqueueEvent projectInfo event = do
        -- Call the corresponding enqueue function if the project exists,
        -- otherwise drop the event on the floor.
        maybe (return ()) (\threadState -> Logic.enqueueEvent (projectThreadQueue threadState) event) $
          Map.lookup projectInfo projectThreadState

  let tryEnqueueProjectEvent projectInfo event = do
        -- Call the corresponding enqueue function if the project exists,
        -- otherwise drop the event on the floor.
        maybe (return False) (\threadState -> Logic.tryEnqueueEvent (projectThreadQueue threadState) event) $
          Map.lookup projectInfo projectThreadState

  -- Start a worker thread to put the GitHub webhook events in the right queue.
  ghThread <- Async.async $ runStdoutLoggingT $ runGithubEventLoop ghQueue enqueueEvent

  -- Create a thread that will send a clock tick events with an interval. The default interval is 30 seconds.
  let interval = fromMaybe (ClockTickInterval 30) (Config.clockTickInterval config)
  timerThread <- Async.async $ clockTickLoop interval (map projectThreadQueue (Map.elems projectThreadState))

  -- Start a main event loop for every project.
  metrics <- Metrics.registerProjectMetrics
  Metrics.registerGHCMetrics
  projectThreads <- forM (map snd (Map.toList projectThreadState)) $ projectThread config options metrics

  let
    -- When the webhook server receives an event, enqueue it on the webhook
    -- event queue if it is not full. If it is, then we'll print that to STDOUT.
    ghTryEnqueue event = do
      result <- Github.tryEnqueueEvent ghQueue event
      unless result $
        putStrLn $
          "The GitHub queue is full, dropped the following webhook event: " <> show event

      pure result

    -- Allow the webinterface to retrieve the latest project state per project.
    readProjectState = Logic.readStateVar . projectThreadStateVar
    getProjectState projectInfo = readProjectState <$> Map.lookup projectInfo projectThreadState
    getOwnerState :: Owner -> IO [(ProjectInfo, ProjectState)]
    getOwnerState owner =
      Map.toList <$> mapM readProjectState (subMapByOwner owner projectThreadState)

  let
    port = Config.port config
    tlsConfig = Config.tls config
    secret = Config.secret config
    -- TODO: Do this in a cleaner way.
    infos = getProjectInfo <$> Config.projects config
  putStrLn $ "Listening for webhooks on port " ++ show port ++ "."
  runServer <- fst <$> buildServer port tlsConfig infos secret ghTryEnqueue tryEnqueueProjectEvent getProjectState getOwnerState
  serverThread <- Async.async runServer
  metricsThread <- runMetricsThread config

  -- Note that a stop signal is never enqueued. The application just runs until
  -- until it is killed, or until any of the threads stop due to an exception.
  void $ Async.waitAny $ [serverThread, ghThread, timerThread] ++ metricsThread ++ projectThreads

data ProjectThreadData = ProjectThreadData
  { projectThreadConfig :: Config.ProjectConfiguration
  , projectThreadQueue :: Logic.EventQueue
  , projectThreadStateVar :: Logic.StateVar
  }

projectThread
  :: Configuration
  -> Options
  -> Metrics.ProjectMetrics
  -> ProjectThreadData
  -> IO (Async.Async ())
projectThread config options metrics projectThreadData = do
  -- At startup, enqueue a synchronize event. This will bring the state in
  -- sync with the current state of GitHub, accounting for any webhooks that
  -- we missed while not running, or just to fill the state initially after
  -- setting up a new project.
  liftIO $ Logic.enqueueEvent projectQueue Logic.Synchronize
  projectThreadState <- Logic.readStateVar $ projectThreadStateVar projectThreadData
  -- Start a worker thread to run the main event loop for the project.
  Async.async $
    void $
      runEff $
        runMetrics $
          runTime $
            runLoggerStdout $
              runGit $
                runGithub $
                  runLogicEventLoop
                    (Config.trigger config)
                    projectConfig
                    (Config.mergeWindowExemption config)
                    (Config.featureFreezeWindow config)
                    (Config.timeouts config)
                    getNextEvent
                    publish
                    projectThreadState
 where
  -- When the event loop publishes the current project state, save it to
  -- the configured file, and make the new state available to the
  -- webinterface.
  projectConfig = projectThreadConfig projectThreadData
  projectQueue = projectThreadQueue projectThreadData
  publish newState = do
    liftIO $ saveProjectState (Config.stateFile projectConfig) newState
    liftIO $ Logic.updateStateVar (projectThreadStateVar projectThreadData) newState

  -- When the event loop wants to get the next event, take one off the queue.
  getNextEvent = liftIO $ Logic.dequeueEvent projectQueue

  -- In the production app, we interpret both Git actions and GitHub actions
  -- to the real thing in IO. In the tests, when calling `runLogicEventLoop`
  -- we could swap one or both of them out for a test implementation.
  repoDir = Config.checkout projectConfig
  auth = Github3.OAuth $ Text.encodeUtf8 $ Config.accessToken config
  projectInfo = ProjectInfo (Config.owner projectConfig) (Config.repository projectConfig)
  runGit =
    if readOnly options
      then Git.runGitReadOnly (Config.user config) repoDir
      else Git.runGit (Config.user config) repoDir
  runGithub =
    if readOnly options
      then GithubApi.runGithubReadOnly auth projectInfo
      else GithubApi.runGithub auth projectInfo
  runTime = Time.runTime
  runMetrics = Metrics.runMetrics metrics $ Config.repository projectConfig

runMetricsThread :: Configuration -> IO [Async.Async ()]
runMetricsThread configuration =
  forM (maybeToList $ Config.metricsConfig configuration) $
    \metricsConf -> do
      let servConfig =
            MetricsServerConfig
              { metricsConfigPort = metricsPort metricsConf
              , metricsConfigHost = fromString $ Text.unpack $ metricsHost metricsConf
              }
      Async.async $ runMetricsServer servConfig
