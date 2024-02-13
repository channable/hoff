{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- This file contains sort-of end-to-end tests for the event loop. Incoming
-- messages are faked (supplied by the test code, as oppsed to received via the
-- web server), but the event loop does run with actual IO with a test
-- repository. The purpose of these tests is not to test the *logic* of the
-- event loop (there are unit tests for that), but to test that the interaction
-- with a real Git process works as expected.

module EventLoopSpec (eventLoopSpec) where

import Control.Concurrent.Async (async, wait)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Effectful (Eff, runEff)
import Prelude hiding (appendFile, writeFile)
import System.FilePath ((</>))
import Test.Hspec

import qualified System.Directory as FileSystem
import qualified Data.UUID.V4 as Uuid
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Time as T
import qualified Data.Time.Calendar.OrdinalDate as T

import Configuration (ProjectConfiguration, TriggerConfiguration, UserConfiguration, MergeWindowExemptionConfiguration (..), FeatureFreezeWindow)
import Git (BaseBranch (..), Branch (..), RefSpec (refSpec), Sha (..))
import Metrics.Metrics (MetricsOperation (..))
import Project (BuildStatus (..), IntegrationStatus (..), ProjectState, PullRequestId (..))

import qualified Configuration as Config
import qualified Data.Text as Text
import qualified EventLoop
import qualified Git
import qualified GithubApi
import qualified Logic
import qualified Prelude
import qualified Project
import qualified Time
import Effectful.Dispatch.Dynamic (interpret)
import MonadLoggerEffect (MonadLoggerEffect (..))

masterBranch :: BaseBranch
masterBranch = BaseBranch "master"

-- Invokes Git with the given arguments, returns its stdout. Crashes if invoking
-- Git failed. Discards all logging.
callGit :: [String] -> IO Text
callGit args = fmap (either undefined id) $ runEff $ fakeRunLogger $ Git.callGit userConfig args

-- | Populates the repository with the following history:
--
--                 .-- c5 -- c6  <-- intro (pr 6)
--                /
--   c0 -- c1 -- c2 -- c3 -- c4  <-- ahead (pr 4)
--        <v1>    \     ^----------- master
--                 `-- c3'       <-- alternative (pr 3)
--
--   c6 -- c7 -- c8 -- c7f <-------- fixup (pr 8)
--
--
-- The given clone action is called when the repository
-- is only partially populated with:
--
--   c0 -- c1 -- c2 -- c3
populateRepository :: FilePath -> IO () -> IO [Sha]
populateRepository dir doClone =
  let writeFile fname msg  = Prelude.writeFile (dir </> fname) (msg ++ "\n")
      appendFile fname msg = Prelude.appendFile (dir </> fname) (msg ++ "\n")
      git args             = callGit $ ["-C", dir] ++ args
      gitInit              = void $ git ["init"]
      gitConfig key value  = void $ git ["config", key, value]
      gitAdd file          = void $ git ["add", file]
      gitBranch name sha   = void $ git ["checkout", "-b", name, refSpec sha]
      gitSetRef name sha   = void $ git ["update-ref", name, refSpec sha]
      getHeadSha           = Sha . Text.stripEnd <$> git ["rev-parse", "@"]
      -- Commits with the given message and returns the sha of the new commit.
      gitCommit message    = git ["commit", "-m", message] >> getHeadSha
  in  do
      gitInit
      gitConfig "user.email" "testsuite@example.com"
      gitConfig "user.name" "Testbot"

      writeFile "tyrell.txt" "I'm surprised you didn't come here sooner."
      gitAdd "tyrell.txt"
      c0 <- gitCommit "c0: Initial commit"

      writeFile "roy.txt" "It's not an easy thing to meet your maker."
      gitAdd "roy.txt"
      c1 <- gitCommit "c1: Add new quote"
      void $ git ["tag", "-a", "v1", "-m", "v1", refSpec c1]

      appendFile "tyrell.txt" "What can he do for you?"
      gitAdd "tyrell.txt"
      c2 <- gitCommit "c2: Add new Tyrell quote"

      appendFile "roy.txt" "Can the maker repair what he makes?"
      gitAdd "roy.txt"
      c3 <- gitCommit "c3: Add new Roy quote"

      doClone

      -- Create a branch "ahead", one commit ahead of master.
      gitBranch "ahead" c3
      appendFile "tyrell.txt" "Would you like to be modified?"
      gitAdd "tyrell.txt"
      c4 <- gitCommit "c4: Add Tyrell response"

      -- Now make an alternative commit that conflicts with c3.
      gitBranch "alternative" c2
      appendFile "roy.txt" "You could make me a sandwich."
      gitAdd "roy.txt"
      c3' <- gitCommit "c3': Write alternative ending"

      -- Also add a commit that does not conflict.
      gitBranch "intro" c2
      writeFile "leon.txt" "What do you mean, I'm not helping?"
      gitAdd "leon.txt"
      c5 <- gitCommit "c5: Add more characters"

      writeFile "holden.txt" "I mean, you're not helping! Why is that, Leon?"
      gitAdd "holden.txt"
      c6 <- gitCommit "c6: Add response"

      -- Make a chain with a fixup commit.
      gitBranch "fixup" c6
      appendFile "holden.txt" "They're just questions Leon."
      gitAdd "holden.txt"
      c7 <- gitCommit "c7: Elaborate on response"

      appendFile "leon.txt" "Do you make up these questions, Mr. Holden?"
      gitAdd "leon.txt"
      c8 <- gitCommit "c8: Add question (not in chronological order)"

      appendFile "holden.txt" "It's a test, designed to provoke an emotional response."
      gitAdd "holden.txt"
      -- There is nothing special about a fixup commit, apart from the message
      -- starting with "fixup!". Rather than committing with --fixup, we can
      -- just generate that message manually here.
      c7f <- gitCommit "fixup! c7: Elaborate on response"

      -- Switch to a branch that is not otherwise used. Because the repository
      -- is not bare, a push to a branch that we have checked out may fail, so
      -- we check out a branch that is never pushed to.
      gitBranch "unused" c3

      -- Assign a pull request ref to some commits, like how they would exist
      -- on GitHub. This enables fetching them later.
      gitSetRef "refs/pull/3/head" c3'
      gitSetRef "refs/pull/4/head" c4
      gitSetRef "refs/pull/6/head" c6
      gitSetRef "refs/pull/8/head" c7f

      return [c0, c1, c2, c3, c3', c4, c5, c6, c7, c7f, c8]

-- Sets up two repositories: one with a few commits in the origin directory, and
-- a clone of that in the repository directory. The clone ensures that the
-- origin repository is set as the "origin" remote in the cloned repository.
initializeRepository :: FilePath -> FilePath -> IO [Sha]
initializeRepository originDir repoDir = do
  -- Create the directory for the origin repository, and parent directories.
  FileSystem.createDirectoryIfMissing True originDir
  -- Populates the repository.  The function 'populateRepository' calls the
  -- given callback action in an early point in history before commits in
  -- branches other than master are in the repo dir: when this is running for
  -- real, we won't have new commits already in the repository either. They
  -- need to be fetched.
  shas <- populateRepository originDir (void $ callGit ["clone", "file://" ++ originDir, repoDir])
  -- Set the author details in the cloned repository as well, to ensure that
  -- there is no implicit dependency on a global Git configuration.
  _    <- callGit ["-C", repoDir, "config", "user.email", "testsuite@example.com"]
  _    <- callGit ["-C", repoDir, "config", "user.name", "Testbot"]
  return shas

-- Generate a project configuration to be used in the test environment.
buildProjectConfig :: FilePath -> FilePath -> ProjectConfiguration
buildProjectConfig repoDir stateFile = Config.ProjectConfiguration {
  Config.owner              = "ruuda",
  Config.repository         = "blog",
  Config.branch             = "master",
  Config.testBranch         = "integration",
  Config.checkout           = repoDir,
  Config.stateFile          = stateFile,
  Config.checks             = Just (Config.ChecksConfiguration Set.empty),
  Config.deployEnvironments = Just ["staging", "production"],
  Config.deploySubprojects  = Nothing
}

-- Dummy user configuration used in test environment.
userConfig :: UserConfiguration
userConfig = Config.UserConfiguration {
  Config.name          = "A Blade Runner",
  Config.email         = "automation@tyrell.com",
  Config.sshConfigFile = "/outerspace/.ssh/config"
}

-- Dummy trigger configuration used in the test environment.
triggerConfig :: TriggerConfiguration
triggerConfig = Config.TriggerConfiguration {
  Config.commentPrefix = "@bot"
}

mergeWindowExemptionConfig :: MergeWindowExemptionConfiguration
mergeWindowExemptionConfig = MergeWindowExemptionConfiguration ["bot"]

featureFreezeWindow :: Maybe FeatureFreezeWindow
featureFreezeWindow = Nothing

testPromotionTimeout :: Config.PromotionTimeout
testPromotionTimeout = Config.PromotionTimeout 600

-- An interpreter for the GitHub API free monad that ignores most API calls, and
-- provides fake inputs. We don't want to require a Github repository and API
-- token to be able to run the tests, and that we send the right operations is
-- checked by the unit tests.
fakeRunGithub :: Eff (GithubApi.GithubOperation : es) a -> Eff es a
fakeRunGithub = interpret $ \_ -> \case
  GithubApi.LeaveComment _pr _body -> pure ()
  GithubApi.HasPushAccess username -> pure $ username `elem` ["rachael", "deckard"]
  -- Pretend that these two GitHub API calls always fail in these tests.
  GithubApi.GetPullRequest _pr -> pure Nothing
  GithubApi.GetOpenPullRequests -> pure Nothing

fakeRunTime :: Eff (Time.TimeOperation : es) a -> Eff es a
fakeRunTime = interpret $ \_ -> \case
  Time.GetDateTime -> pure $ T.UTCTime (T.fromMondayStartWeek 2021 2 1) (T.secondsToDiffTime 0)

fakeRunMetrics :: Eff (MetricsOperation : es) a -> Eff es a
fakeRunMetrics = interpret $ \_ -> \case
  MergeBranch -> pure ()
  UpdateTrainSize _ -> pure ()

fakeRunLogger :: Eff (MonadLoggerEffect : es) a -> Eff es a
fakeRunLogger = interpret $ \_ -> \case
  MonadLoggerLog _loc _logSource _logLevel _msg -> pure ()

-- Runs the main loop in a separate thread, and feeds it the given events.
runMainEventLoop
  :: ProjectConfiguration
  -> ProjectState
  -> [Logic.Event]
  -> IO ProjectState
runMainEventLoop projectConfig initialState events = do
  -- Like the actual application, start a new thread to run the main event loop.
  -- Use 'async' here, a higher-level wrapper around 'forkIO', to wait for the
  -- thread to stop later. Discard log messages from the event loop, to avoid
  -- polluting the test output.
  --
  -- To aid debugging when a test fails, you can replace 'fakeRunLogger' with
  -- 'runLoggerStdout'. You should also remove 'parallel' from main then.
  queue <- Logic.newEventQueue 10
  let
    publish _    = return () -- Do nothing when a new state is published.
    getNextEvent = liftIO $ Logic.dequeueEvent queue
    runMetrics   = fakeRunMetrics
    runGit       = Git.runGit userConfig (Config.checkout projectConfig)
    runGithub    = fakeRunGithub
    runTime      = fakeRunTime
  finalStateAsync <- async
    $ runEff
    $ fakeRunLogger
    $ runMetrics
    $ runTime
    $ runGit
    $ runGithub
    $ EventLoop.runLogicEventLoop
        triggerConfig
        projectConfig
        mergeWindowExemptionConfig
        featureFreezeWindow
        testPromotionTimeout
        getNextEvent
        publish
        initialState

  -- Enqueue all provided events.
  forM_ events (Logic.enqueueEvent queue)

  -- Tell the worker thread to stop after it has processed all events. Then wait
  -- for it to exit, and return the final state.
  Logic.enqueueStopSignal queue
  wait finalStateAsync

-- Recursively makes all files and directories in a directory writable.
-- On Windows this is required to be able to recursively delete the directory.
makeWritableRecursive :: FilePath -> IO ()
makeWritableRecursive path = do
  permissions <- FileSystem.getPermissions path
  FileSystem.setPermissions path (FileSystem.setOwnerWritable True permissions)
  isDirectory <- FileSystem.doesDirectoryExist path
  when isDirectory $ do
    contents <- FileSystem.listDirectory path
    forM_ contents $ \ item -> makeWritableRecursive (path </> item)

type LoopRunner = ProjectState -> [Logic.Event] -> IO ProjectState
type GitRunner = [String] -> IO ()

-- | Tag annotations contain a shortlog of the commits between the current and previous versions.
data TagAnn = TagAnn {
  tagName     :: Text,
  tagSubject  :: Text,
  tagBody     :: Map Text (Set Text)
} deriving (Eq, Show)

-- Sets up a test environment with an actual Git repository on the file system,
-- and a thread running the main event loop. Then invokes the body, and tears
-- down the test environment afterwards. Returns a list of commit message
-- prefixes of the remote master branch log. The body function is provided with
-- the shas of the test repository and a function to run the event loop.
withTestEnv :: ([Sha] -> LoopRunner -> GitRunner -> IO ()) -> IO [Text]
withTestEnv = fmap (\(history, _branches, _tagRefs, _tagAnns) -> history) . withTestEnv'

-- Sets up a test environment with an actual Git repository on the file system,
-- and a thread running the main event loop. Then invokes the body, and tears
-- down the test environment afterwards. Returns a list of commit message
-- prefixes of the remote master branch log, and also all remote branches. The
-- body function is provided with the shas of the test repository and a function
-- to run the event loop.
withTestEnv'
  :: ([Sha] -> LoopRunner -> GitRunner -> IO ())
  -> IO ([Text], [Branch], [Text], [TagAnn])
withTestEnv' body = do
  -- To run these tests, a real repository has to be made somewhere. Do that in
  -- /tmp because it can be mounted as a ramdisk, so it is fast and we don't
  -- unnecessarily wear out SSDs. Put a uuid in there to ensure we don't
  -- overwrite somebody else's files, and to ensure that the tests do not affect
  -- eachother.
  uuid       <- Uuid.nextRandom
  tmpBaseDir <- FileSystem.getTemporaryDirectory
  let
    testDir   = tmpBaseDir </> ("testsuite-" ++ show uuid)
    originDir = testDir </> "repo-origin"
    repoDir   = testDir </> "repo-local"
    stateFile = testDir </> "state.json"
  -- Create and populate a test repository with a local remote "origin". Record
  -- the shas of the commits as documented in populateRepository.
  shas <- initializeRepository originDir repoDir

  -- Run the actual test code inside the environment that we just set up,
  -- provide it with the commit shas, the function to run the event loop, and a
  -- function to invoke Git in the cloned repository.
  let
    projectConfig = buildProjectConfig repoDir stateFile
    git args = void $ callGit $ ["-C", repoDir] ++ args
  body shas (runMainEventLoop projectConfig) git

  -- Retrieve the log of the remote repository master branch. Only show the
  -- commit message subject lines. The repository has been setup to prefix
  -- messages with a commit number followed by a colon. Strip off the rest.
  -- Commit messages are compared later, rather than shas, because these do not
  -- change when rebased, and they do not depend on the current timestamp.
  -- (Commits do: the same rebase operation can produce commits with different
  -- shas depending on the time of the rebase.)
  masterLog <- callGit ["-C", originDir, "log", "--format=%s", "--graph", "master"]
  branchesRaw <- callGit ["-C", originDir, "branch", "--format=%(refname:short)"]
  -- Tags are sorted in no particular order and displayed as 'tagname commit_number: message'. We
  -- filter out the messages so that only colon numbers remain
  tagReferencesRaw <- callGit ["-C", originDir, "log", "--tags", "--no-walk", "--format=%S %s"]
  tagAnnotationsRaw <- callGit ["-C", originDir, "tag", "--list", "-n100"]
  let
    commits = stripMessage <$> Text.lines masterLog
    branches = Branch <$> Text.lines branchesRaw
    tagReferences = stripMessage <$> Text.lines tagReferencesRaw
    tagAnnotations = (groupAnnotations . Text.lines) tagAnnotationsRaw

  makeWritableRecursive testDir
  FileSystem.removeDirectoryRecursive testDir
  pure (commits, branches, tagReferences, tagAnnotations)
  where
    stripMessage = Text.strip . Text.takeWhile (/= ':')

    -- Group annotations
    -- `git tag` doesn't support `-z` option to split the groups by a null byte.
    -- We have to split the different annotations on indentation...
    groupAnnotations :: [Text] -> [TagAnn]
    groupAnnotations [] = []
    groupAnnotations (x:xs) =
      TagAnn tag (Text.strip subject) (groupCommits $ map (Text.drop 4) ys) : groupAnnotations zs
      where
        (tag, subject) = Text.breakOn " " x
        (ys, zs) = span (Text.isPrefixOf "    ") xs

    -- Group commits by committer
    -- The committers are ordered alphabetically.
    -- The order of the commits is unspecified.
    groupCommits :: [Text] -> Map Text (Set Text)
    groupCommits [] = Map.empty
    groupCommits ("":xs) = groupCommits xs
    groupCommits (x:xs) = Map.insert x ((Set.fromList . map Text.strip) ys) (groupCommits zs)
      where
        (ys, zs) = span (Text.isPrefixOf "    ") xs

-- | lists the integration Shas from the state for all PRs which are Integrated
integrationShas :: ProjectState -> [Sha]
integrationShas state = [ sha
                        | prId <- Project.unfailedIntegratedPullRequests state
                        , Just pr <- [Project.lookupPullRequest prId state]
                        , Integrated sha _ <- [Project.integrationStatus pr]
                        ]

eventLoopSpec :: Spec
eventLoopSpec = parallel $ do
  describe "The main event loop" $ do

    it "handles a fast-forwardable pull request" $ do
      (history, branches, _tagRefs, _tagAnns) <- withTestEnv' $ \ shas runLoop _git -> do
        let
          [_c0, _c1, _c2, _c3, _c3', c4, _c5, _c6, _c7, _c7f, _c8] = shas
          -- Note that at the remote, refs/pull/4/head points to c4.
          pr4 = PullRequestId 4
          branch = Branch "ahead"
          baseBranch = masterBranch

        -- Commit c4 is one commit ahead of master, so integrating it can be done
        -- with a fast-forward merge. Run the main event loop for these events
        -- and discard the final state by using 'void'.
        void $ runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr4 branch baseBranch c4 "Add Leon test results" "deckard" Nothing,
            Logic.CommentAdded pr4 "rachael" "@bot merge",
            Logic.BuildStatusChanged c4 "default" BuildSucceeded,
            Logic.PullRequestCommitChanged (PullRequestId 4) c4
          ]
      history `shouldBe`
        [ "* c4"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      -- The remote branch ("ahead") will still be present here,
      -- but will be deleted by GitHub (if configured to do so)
      -- if there are no other PRs depending on it.
      -- The other branches should be left untouched.
      branches `shouldMatchList`
        fmap Branch ["ahead", "intro", "master", "alternative", "fixup", "unused"]

    it "handles a fast-forwardable pull request from pull request description" $ do
      (history, branches, _tagRefs, _tagAnns) <- withTestEnv' $ \ shas runLoop _git -> do
        let
          [_c0, _c1, _c2, _c3, _c3', c4, _c5, _c6, _c7, _c7f, _c8] = shas
          -- Note that at the remote, refs/pull/4/head points to c4.
          pr4 = PullRequestId 4
          branch = Branch "ahead"
          baseBranch = masterBranch

        -- Commit c4 is one commit ahead of master, so integrating it can be done
        -- with a fast-forward merge. Run the main event loop for these events
        -- and discard the final state by using 'void'.
        void $ runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr4 branch baseBranch c4 "Add Leon test results" "deckard" (Just "@bot merge"),
            Logic.BuildStatusChanged c4 "default" BuildSucceeded,
            Logic.PullRequestCommitChanged (PullRequestId 4) c4
          ]
      history `shouldBe`
        [ "* c4"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      -- The remote branch ("ahead") will still be present here,
      -- but will be deleted by GitHub (if configured to do so)
      -- if there are no other PRs depending on it.
      -- The other branches should be left untouched.
      branches `shouldMatchList`
        fmap Branch ["ahead", "intro", "master", "alternative", "fixup", "unused"]

    it "keeps the integration test branch on a failing build" $ do
      (history, branches, _tagRefs, _tagAnns) <- withTestEnv' $ \ shas runLoop _git -> do
        let
          [_c0, _c1, _c2, _c3, _c3', c4, _c5, _c6, _c7, _c7f, _c8] = shas
          -- Note that at the remote, refs/pull/4/head points to c4.
          pr4 = PullRequestId 4
          branch = Branch "ahead"
          baseBranch = masterBranch

        void $ runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr4 branch baseBranch c4 "Add Leon test results" "deckard" Nothing,
            Logic.CommentAdded pr4 "rachael" "@bot merge",
            Logic.BuildStatusChanged c4 "default" (BuildFailed Nothing)
          ]
      -- the build failed, so master's history is unchanged
      -- ... and the integration/4 branch is kept for inpection of the CI build
      history `shouldBe`
        [ "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      branches `shouldMatchList`
        fmap Branch ["ahead", "intro", "master", "alternative", "fixup", "unused", "integration/4"]

    it "handles a fast-forwardable pull request with tag" $ do
      (history, _branches, tagRefs, tagAnns) <- withTestEnv' $ \ shas runLoop _git -> do
        let
          [_c0, _c1, _c2, _c3, _c3', c4, _c5, _c6, _c7, _c7f, _c8] = shas
          pr4 = PullRequestId 4
          branch = Branch "ahead"
          baseBranch = masterBranch

        -- Commit c4 is one commit ahead of master, but it is marked for "merge
        -- and tag", so a merge commit must be created.
        -- A new tag `v2` should appear on the merge commit.
        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr4 branch baseBranch c4 "Deploy tests!" "deckard" Nothing,
            Logic.CommentAdded pr4 "rachael" "@bot merge and tag",
            Logic.BuildStatusChanged c4 "default" BuildSucceeded
          ]

        let [rebasedSha] = integrationShas state

        state' <- runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
                                 Logic.PullRequestCommitChanged (PullRequestId 4) rebasedSha]

        let [rebasedSha'] = integrationShas state'
        void $ runLoop state' [Logic.BuildStatusChanged rebasedSha' "default" BuildSucceeded,
                               Logic.PullRequestCommitChanged (PullRequestId 4) rebasedSha]
      history `shouldBe`
        [ "*   Merge #4"
        , "|\\"
        , "| * c4"
        , "|/"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      tagRefs `shouldMatchList`
        [ "v1 c1"
        , "v2 Merge #4"
        ]
      tagAnns `shouldMatchList`
        [ TagAnn
            { tagName = "v2"
            , tagSubject = "v2"
            , tagBody = Map.fromList
                [ ( "Testbot (4):", Set.fromList
                    [ "Merge #4: Deploy tests!"
                    , "c2: Add new Tyrell quote"
                    , "c3: Add new Roy quote"
                    , "c4: Add Tyrell response"
                    ]
                  )
                ]
            }
        , TagAnn
            { tagName = "v1"
            , tagSubject = "v1"
            , tagBody = Map.empty
            }
        ]

    it "handles a fast-forwardable pull request with deploy" $ do
      (history, _branches, tagRefs, tagAnns) <- withTestEnv' $ \ shas runLoop _git -> do
        let
          [_c0, _c1, _c2, _c3, _c3', c4, _c5, _c6, _c7, _c7f, _c8] = shas
          pr4 = PullRequestId 4
          branch = Branch "ahead"
          baseBranch = masterBranch

        -- Commit c4 is one commit ahead of master, so integrating it can be done
        -- with a fast-forward merge. The deploy command currently enforces a merge
        -- commit. A new tag `v2` should appear.
        state <- runLoop Project.emptyProjectState
          [ Logic.PullRequestOpened pr4 branch baseBranch c4 "Deploy tests!" "deckard" Nothing
          , Logic.CommentAdded pr4 "rachael" "@bot merge and deploy to staging"
          ]

        -- Extract the sha of the rebased commit from the project state.
        let [rebasedSha] = integrationShas state

        -- The rebased commit should have been pushed to the remote repository
        -- 'integration' branch. Tell that building it succeeded.
        void $ runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
                              Logic.PullRequestCommitChanged (PullRequestId 4) rebasedSha]

      history `shouldBe`
        [ "*   Merge #4"
        , "|\\"
        , "| * c4"
        , "|/"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      tagRefs `shouldMatchList`
        [ "v2 Merge #4"
        , "v1 c1"
        ]
      tagAnns `shouldMatchList`
        [ TagAnn
            { tagName = "v2"
            , tagSubject = "v2 (autodeploy)"
            , tagBody = Map.fromList
                [ ( "Testbot (4):", Set.fromList
                    [ "c2: Add new Tyrell quote"
                    , "c3: Add new Roy quote"
                    , "c4: Add Tyrell response"
                    , "Merge #4: Deploy tests!"
                    ]
                  )
                ]
            }
        , TagAnn
            { tagName = "v1"
            , tagSubject = "v1"
            , tagBody = Map.empty
            }
        ]

    it "handles a non-conflicting non-fast-forwardable pull request" $ do
      (history, branches, _tagRefs, _tagAnns) <- withTestEnv' $ \ shas runLoop _git -> do
        let [_c0, _c1, _c2, _c3, _c3', _c4, _c5, c6, _c7, _c7f, _c8] = shas
            -- Note that at the remote, refs/pull/6/head points to c6.
            pr6 = PullRequestId 6
            branch = Branch "intro"
            baseBranch = masterBranch

        -- Commit c6 is two commits ahead and one behind of master, so
        -- integrating it produces new rebased commits.
        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr6 branch baseBranch c6 "Add Leon test results" "deckard" Nothing,
            Logic.CommentAdded pr6 "rachael" "@bot merge"
          ]

        -- Extract the sha of the rebased commit from the project state.
        let [rebasedSha] = integrationShas state

        -- The rebased commit should have been pushed to the remote repository
        -- 'integration' branch. Tell that building it succeeded.
        void $ runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
                              Logic.PullRequestCommitChanged (PullRequestId 6) rebasedSha]

      history `shouldBe`
        [ "*   Merge #6"
        , "|\\"
        , "| * c6"
        , "| * c5"
        , "|/"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      -- The remote branch ("intro") will still be present here,
      -- but will be deleted by GitHub (if configured to do so)
      -- if there are no other PRs depending on it.
      -- The other branches should be left untouched.
      branches `shouldMatchList`
        fmap Branch ["ahead", "intro", "master", "alternative", "fixup", "unused"]

    it "handles a non-conflicting non-fast-forwardable pull request with tag" $ do
      (history, _branches, tagRefs, tagAnns) <- withTestEnv' $ \ shas runLoop _git -> do
        let [_c0, _c1, _c2, _c3, _c3', _c4, _c5, c6, _c7, _c7f, _c8] = shas
            pr6 = PullRequestId 6
            branch = Branch "intro"
            baseBranch = masterBranch

        -- Commit c6 is two commits ahead and one behind of master, so
        -- integrating it produces new rebased commits.
        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr6 branch baseBranch c6 "Deploy it now!" "deckard" Nothing,
            Logic.CommentAdded pr6 "rachael" "@bot merge and tag"
          ]

        let [rebasedSha] = integrationShas state

        void $ runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
                              Logic.PullRequestCommitChanged (PullRequestId 6) rebasedSha]

      history `shouldBe`
        [ "*   Merge #6"
        , "|\\"
        , "| * c6"
        , "| * c5"
        , "|/"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      tagRefs `shouldMatchList`
        [ "v2 Merge #6"
        , "v1 c1"
        ]
      tagAnns `shouldMatchList`
        [ TagAnn
            { tagName = "v2"
            , tagSubject = "v2"
            , tagBody = Map.fromList
                [ ( "Testbot (5):"
                  , Set.fromList
                      [ "c2: Add new Tyrell quote"
                      , "c3: Add new Roy quote"
                      , "c5: Add more characters"
                      , "c6: Add response"
                      , "Merge #6: Deploy it now!"
                      ]
                  )
                ]
            }
        , TagAnn
            { tagName = "v1"
            , tagSubject = "v1"
            , tagBody = Map.empty
            }
        ]


    it "handles a non-conflicting non-fast-forwardable pull request with deploy" $ do
      (history, _branches, tagRefs, tagAnns) <- withTestEnv' $ \ shas runLoop _git -> do
        let [_c0, _c1, _c2, _c3, _c3', _c4, _c5, c6, _c7, _c7f, _c8] = shas
            pr6 = PullRequestId 6
            branch = Branch "intro"
            baseBranch = masterBranch

        -- Commit c6 is two commits ahead and one behind of master, so
        -- integrating it produces new rebased commits.
        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr6 branch baseBranch c6 "Deploy it now!" "deckard" Nothing,
            Logic.CommentAdded pr6 "rachael" "@bot merge and deploy to staging"
          ]

        let [rebasedSha] = integrationShas state

        void $ runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
                              Logic.PullRequestCommitChanged (PullRequestId 6) rebasedSha]

      history `shouldBe`
        [ "*   Merge #6"
        , "|\\"
        , "| * c6"
        , "| * c5"
        , "|/"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      tagRefs `shouldMatchList`
        [ "v2 Merge #6"
        , "v1 c1"
        ]
      tagAnns `shouldMatchList`
        [ TagAnn
            { tagName = "v2"
            , tagSubject = "v2 (autodeploy)"
            , tagBody = Map.fromList
                [ ( "Testbot (5):", Set.fromList
                    [ "c2: Add new Tyrell quote"
                    , "c3: Add new Roy quote"
                    , "c5: Add more characters"
                    , "c6: Add response"
                    , "Merge #6: Deploy it now!"
                    ]
                  )
                ]
            }
        , TagAnn
            { tagName = "v1"
            , tagSubject = "v1"
            , tagBody = Map.empty
            }
        ]


    it "handles multiple pull requests" $ do
      history <- withTestEnv $ \ shas runLoop _git -> do
        let [_c0, _c1, _c2, _c3, _c3', c4, _c5, c6, _c7, _c7f, _c8] = shas
            pr4 = PullRequestId 4
            pr6 = PullRequestId 6
            br4 = Branch "ahead"
            br6 = Branch "intro"
            baseBranch = masterBranch

        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr4 br4 baseBranch c4 "Add Leon test results" "deckard" Nothing,
            Logic.PullRequestOpened pr6 br6 baseBranch c6 "Add Rachael test results" "deckard" Nothing,
            -- Note that although c4 has a lower pull request number, c6 should
            -- still be integrated first because it was approved earlier.
            Logic.CommentAdded pr6 "rachael" "@bot merge",
            Logic.CommentAdded pr4 "rachael" "@bot merge"
          ]

        -- Extract the sha of the rebased commit from the project state.
        let [rebasedSha,_] = integrationShas state

        -- The rebased commit should have been pushed to the remote repository
        -- 'integration' branch. Tell that building it succeeded.
        state' <- runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
                                 Logic.PullRequestCommitChanged (PullRequestId 6) rebasedSha]

        -- Repeat for the other pull request, which should be the candidate by
        -- now.
        let [rebasedSha'] = integrationShas state'
        void $ runLoop state' [Logic.BuildStatusChanged rebasedSha' "default" BuildSucceeded,
                               Logic.PullRequestCommitChanged (PullRequestId 4) rebasedSha']

      history `shouldBe`
        [ "* c4"
        , "*   Merge #6"
        , "|\\"
        , "| * c6"
        , "| * c5"
        , "|/"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]

    it "tags version consistently across multiple PRs" $ do
      (history, _branches, tagRefs, tagAnns) <- withTestEnv' $ \ shas runLoop _git -> do
        let [_c0, _c1, _c2, _c3, _c3', c4, _c5, c6, _c7, _c7f, _c8] = shas
            pr4 = PullRequestId 4
            pr6 = PullRequestId 6
            br4 = Branch "ahead"
            br6 = Branch "intro"
            baseBranch = masterBranch

        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr4 br4 baseBranch c4 "Add Leon test results" "deckard" Nothing,
            Logic.PullRequestOpened pr6 br6 baseBranch c6 "Add Rachael test results" "deckard" Nothing,
            Logic.CommentAdded pr6 "rachael" "@bot merge and tag",
            Logic.CommentAdded pr4 "rachael" "@bot merge and tag"
          ]

        let [rebasedSha,_] = integrationShas state

        state' <- runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
                                 Logic.PullRequestCommitChanged (PullRequestId 6) rebasedSha]

        let [rebasedSha'] = integrationShas state'
        void $ runLoop state' [Logic.BuildStatusChanged rebasedSha' "default" BuildSucceeded,
                               Logic.PullRequestCommitChanged (PullRequestId 4) rebasedSha']

      history `shouldBe`
        [ "*   Merge #4"
        , "|\\"
        , "| * c4"
        , "|/"
        , "*   Merge #6"
        , "|\\"
        , "| * c6"
        , "| * c5"
        , "|/"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      -- PR 6 had been "built" before PR 4 and version should reflect that
      tagRefs `shouldMatchList`
        [ "v3 Merge #4"
        , "v2 Merge #6"
        , "v1 c1"
        ]
      tagAnns `shouldMatchList`
        [ TagAnn
            { tagName = "v3"
            , tagSubject = "v3"
            , tagBody = Map.fromList
                [ ( "Testbot (2):", Set.fromList
                    [ "Merge #4: Add Leon test results"
                    , "c4: Add Tyrell response"
                    ]
                  )
                ]
            }
        , TagAnn
            { tagName = "v2"
            , tagSubject = "v2"
            , tagBody = Map.fromList
                [ ( "Testbot (5):", Set.fromList
                    [ "c2: Add new Tyrell quote"
                    , "c3: Add new Roy quote"
                    , "c5: Add more characters"
                    , "c6: Add response"
                    , "Merge #6: Add Rachael test results"
                    ]
                  )
                ]
            }
        , TagAnn
            { tagName = "v1"
            , tagSubject = "v1"
            , tagBody = Map.empty
            }
        ]


    it "skips conflicted pull requests" $ do
      (history, branches, _tagRefs, _tagAnns) <- withTestEnv' $ \ shas runLoop _git -> do
        let [_c0, _c1, _c2, _c3, c3', c4, _c5, _c6, _c7, _c7f, _c8] = shas
            pr3 = PullRequestId 3
            pr4 = PullRequestId 4
            br3 = Branch "alternative"
            br4 = Branch "ahead"
            baseBranch = masterBranch

        -- Commit c3' conflicts with master, so a rebase should be attempted, but
        -- because it conflicts, the next pull request should be considered.
        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr3 br3 baseBranch c3' "Add Leon test results" "deckard" Nothing,
            Logic.PullRequestOpened pr4 br4 baseBranch c4 "Add Rachael test results" "deckard" Nothing,
            Logic.CommentAdded pr3 "rachael" "@bot merge",
            Logic.CommentAdded pr4 "rachael" "@bot merge"
          ]

        -- The first pull request should be marked as conflicted. Note: this
        -- test also verifies that the repository is left in a good state after
        -- the conflicted rebase, so that the next commit can be integrated
        -- properly.
        let Just pullRequest3 = Project.lookupPullRequest pr3 state
        Project.integrationStatus pullRequest3 `shouldBe` Conflicted masterBranch Git.RebaseFailed

        -- The second pull request should still be pending, awaiting the build
        -- result.
        Project.unfailedIntegratedPullRequests state `shouldBe` [pr4]
        let Just pullRequest4 = Project.lookupPullRequest pr4 state
            Integrated _ buildStatus = Project.integrationStatus pullRequest4
        -- Expect no CI url
        buildStatus `shouldBe` (Project.AnyCheck BuildPending)

      -- We did not send a build status notification for c4, so it should not
      -- have been integrated.
      history `shouldBe`
        [ "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      -- The conflicted branch should not have been deleted.
      branches `shouldContain` [Branch "alternative"]

    it "restarts the sequence after a rejected push" $ do
      history <- withTestEnv $ \ shas runLoop git -> do
        let
          [_c0, _c1, _c2, _c3, _c3', c4, _c5, c6, _c7, _c7f, _c8] = shas
          pr6 = PullRequestId 6
          branch = Branch "intro"
          baseBranch = masterBranch

        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr6 branch baseBranch c6 "Add test results" "deckard" Nothing,
            Logic.CommentAdded pr6 "rachael" "@bot merge"
          ]

        -- At this point, c6 has been rebased and pushed to the "integration"
        -- branch for building. Before we notify build success, push commmit c4
        -- to the origin "master" branch, so that pushing the rebased c6 will
        -- fail later on.
        git ["fetch", "origin", "ahead"] -- The ref for commit c4.
        git ["push", "origin", refSpec (c4, masterBranch)]

        -- Extract the sha of the rebased commit from the project state, and
        -- tell the loop that building the commit succeeded.
        let [rebasedSha] = integrationShas state
        state' <- runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
                                 Logic.PullRequestCommitChanged (PullRequestId 6) rebasedSha]

        -- The push should have failed, hence there should still be an
        -- integration candidate.
        Project.unfailedIntegratedPullRequests state' `shouldSatisfy` (not . null)

        -- Again notify build success, now for the new commit.
        let [rebasedSha'] = integrationShas state'
        state'' <- runLoop state' [Logic.BuildStatusChanged rebasedSha' "default" BuildSucceeded,
                                   Logic.PullRequestCommitChanged (PullRequestId 6) rebasedSha']

        -- After the second build success, the pull request should have been
        -- integrated properly, so there should not be a new candidate.
        Project.unfailedIntegratedPullRequests state'' `shouldBe` []

      history `shouldBe`
        [ "*   Merge #6"
        , "|\\"
        , "| * c6"
        , "| * c5"
        , "|/"
        , "* c4"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      -- The remote branch will still be present here,
      -- but GitHub will remove it automatically if configured to do so.

    it "pushes tags atomically (rejected push)" $ do
      (history, _branches, tagRefs, tagAnns) <- withTestEnv' $ \ shas runLoop git -> do
        let
          [_c0, _c1, _c2, _c3, _c3', c4, _c5, c6, _c7, _c7f, _c8] = shas
          pr6 = PullRequestId 6
          branch = Branch "intro"
          baseBranch = masterBranch

        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr6 branch baseBranch c6 "Add test results" "deckard" Nothing,
            Logic.CommentAdded pr6 "rachael" "@bot merge and tag"
          ]

        -- At this point, c6 has been rebased and pushed to the "integration"
        -- branch for building. Before we notify build success, push commmit c4
        -- to the origin "master" branch, so that pushing the rebased c6 will
        -- fail later on.
        git ["fetch", "origin", "ahead"] -- The ref for commit c4.
        git ["push", "origin", refSpec (c4, masterBranch)]

        let [rebasedSha] = integrationShas state
        state' <- runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
                                 Logic.PullRequestCommitChanged (PullRequestId 6) rebasedSha]

        -- Again notify build success, now for the new commit.
        let [rebasedSha'] = integrationShas state'
        void $ runLoop state' [Logic.BuildStatusChanged rebasedSha' "default" BuildSucceeded,
                               Logic.PullRequestCommitChanged (PullRequestId 6) rebasedSha']

        -- After the second build success, the pull request should have been
        -- integrated properly, version should be incremented only once

      history `shouldBe`
        [ "*   Merge #6"
        , "|\\"
        , "| * c6"
        , "| * c5"
        , "|/"
        , "* c4"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      tagRefs `shouldMatchList`
        [ "v1 c1"
        , "v2 Merge #6"
        ]
      tagAnns `shouldMatchList`
        [ TagAnn
            { tagName = "v2"
            , tagSubject = "v2"
            , tagBody = Map.fromList
                [ ( "Testbot (6):", Set.fromList
                    [ "c2: Add new Tyrell quote"
                    , "c3: Add new Roy quote"
                    , "c4: Add Tyrell response"
                    , "c5: Add more characters"
                    , "c6: Add response"
                    , "Merge #6: Add test results"
                    ]
                  )
                ]
            }
        , TagAnn
            { tagName = "v1"
            , tagSubject = "v1"
            , tagBody = Map.empty
            }
        ]


    it "pushes tags atomically (new tag appears)" $ do
      (history, _branches, tagRefs, tagAnns) <- withTestEnv' $ \ shas runLoop git -> do
        let
          [_c0, _c1, _c2, _c3, _c3', c4, _c5, c6, _c7, _c7f, _c8] = shas
          pr6 = PullRequestId 6
          branch = Branch "intro"
          baseBranch = masterBranch

        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr6 branch baseBranch c6 "Add test results" "deckard" Nothing,
            Logic.CommentAdded pr6 "rachael" "@bot merge and tag"
          ]

        -- At this point, c6 has been rebased and pushed to the "integration" branch for building.
        -- Before we notify build success, push commmit c4 and a new tag to the origin "master"
        -- branch, so that pushing the rebased c6 will fail later on.
        git ["fetch", "origin", "ahead"] -- The ref for commit c4.
        git ["push", "origin", refSpec (c4, masterBranch)]
        git ["tag", "-a", "v2", "-m", "v2", refSpec c4]
        git ["push", "origin", refSpec (Git.TagName "v2")]

        let [rebasedSha] = integrationShas state
        state' <- runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
                                 Logic.PullRequestCommitChanged (PullRequestId 6) rebasedSha]

        -- Again notify build success, now for the new commit.
        let [rebasedSha'] = integrationShas state'
        void $ runLoop state' [Logic.BuildStatusChanged rebasedSha' "default" BuildSucceeded,
                               Logic.PullRequestCommitChanged (PullRequestId 6) rebasedSha']

        -- After the second build success, the pull request should have been integrated properly,
        -- version should be incremented only once, and follow version that appeared in the meantime

      history `shouldBe`
        [ "*   Merge #6"
        , "|\\"
        , "| * c6"
        , "| * c5"
        , "|/"
        , "* c4"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      tagRefs `shouldMatchList`
        [ "v1 c1"
        , "v2 c4"
        , "v3 Merge #6"
        ]
      tagAnns `shouldMatchList`
        [ TagAnn
            { tagName = "v3"
            , tagSubject = "v3"
            , tagBody = Map.fromList
                [ ( "Testbot (3):", Set.fromList
                    [ "c5: Add more characters"
                    , "c6: Add response"
                    , "Merge #6: Add test results"
                    ]
                  )
                ]
            }
        , TagAnn
            { tagName = "v2"
            , tagSubject = "v2"
            , tagBody = Map.empty
            }
        , TagAnn
            { tagName = "v1"
            , tagSubject = "v1"
            , tagBody = Map.empty
            }
        ]


    it "applies fixup commits during rebase, even if fast forward is possible" $ do
      history <- withTestEnv $ \ shas runLoop _git -> do
        let
          [_c0, _c1, _c2, _c3, _c3', _c4, _c5, _c6, _c7, c7f, _c8] = shas
          pr8 = PullRequestId 8
          branch = Branch "fixup"
          baseBranch = masterBranch

        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr8 branch baseBranch c7f "Add test results" "deckard" Nothing,
            Logic.CommentAdded pr8 "rachael" "@bot merge"
          ]

        -- Extract the sha of the rebased commit from the project state, and
        -- tell the loop that building the commit succeeded.
        let [rebasedSha] = integrationShas state
        void $ runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
                              Logic.PullRequestCommitChanged (PullRequestId 8) rebasedSha]

      -- We expect the fixup commit (which was last) to be squashed into c7, so
      -- now c8 is the last commit, and there are no others. Note that if the
      -- fixup had failed, there would be an extra commit, with fixup in the
      -- title.
      history `shouldBe`
        [ "*   Merge #8"
        , "|\\"
        , "| * c8"
        , "| * c7"
        , "| * c6"
        , "| * c5"
        , "|/"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]

    it "applies fixup commits during rebase, also if a push happened" $ do
      history <- withTestEnv $ \ shas runLoop git -> do
        let
          [_c0, _c1, _c2, _c3, _c3', c4, _c5, _c6, _c7, c7f, _c8] = shas
          pr8 = PullRequestId 8
          branch = Branch "fixup"
          baseBranch = masterBranch

        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr8 branch baseBranch c7f "Add test results" "deckard" Nothing,
            Logic.CommentAdded pr8 "rachael" "@bot merge"
          ]

        git ["fetch", "origin", "ahead"] -- The ref for commit c4.
        git ["push", "origin", refSpec (c4, masterBranch)]

        -- Extract the sha of the rebased commit from the project state, and
        -- tell the loop that building the commit succeeded.
        let [rebasedSha] = integrationShas state
        state' <- runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
                                 Logic.PullRequestCommitChanged (PullRequestId 8) rebasedSha ]

        -- Again notify build success, now for the new commit.
        let [rebasedSha'] = integrationShas state'
        void $ runLoop state' [Logic.BuildStatusChanged rebasedSha' "default" BuildSucceeded,
                               Logic.PullRequestCommitChanged (PullRequestId 8) rebasedSha' ]

      -- We expect the fixup commit (which was last) to be squashed into c7, so
      -- now c8 is the last commit, and there are no others. This time c4 and c5
      -- are included too, because we manually pushed them.
      history `shouldBe`
        [ "*   Merge #8"
        , "|\\"
        , "| * c8"
        , "| * c7"
        , "| * c6"
        , "| * c5"
        , "|/"
        , "* c4"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]

    it "do not merge if there exist fixup commits that do not belong to any other commits" $ do
      history <- withTestEnv $ \ shas runLoop git -> do
        let
          [_c0, _c1, _c2, _c3, _c3', _c4, _c5, _c6, _c7, c7f, c8] = shas
          pr8 = PullRequestId 8
          branch = Branch "fixup"
          baseBranch = masterBranch

        -- The commit graph looks like "c7 -- c8 -- c7f", where c7 needs to be
        -- fixed up. We now already push c8 to master, so the only thing left in
        -- the pull request is the fixup commit, with nothing to fix up, because
        -- the bad commit c7 is already on master. Note that origin/master is
        -- not a parent of c8, so we force-push.
        git ["fetch", "origin", "fixup"] -- The ref for commit c7f.
        git ["push", "--force", "origin", refSpec (c8, masterBranch)]

        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr8 branch baseBranch c7f "Add test results" "deckard" Nothing,
            Logic.CommentAdded pr8 "rachael" "@bot merge"
          ]

        -- Extract the sha of the rebased commit from the project state, and
        -- tell the loop that building the commit succeeded.

        let [rebasedSha] = integrationShas state
        state' <- runLoop state [Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded]

        --The pull request should not be integrated. Moreover, the presence of
        --orphan fixups should make the PR ineligible for being a candidate for integration.
        --That is, we expect no candidates for integration.
        Project.unfailedIntegratedPullRequests state' `shouldBe` []

      -- Here we expect that the fixup commit is not present.
      history `shouldBe`
        [ "* c8"
        , "* c7"
        , "* c6"
        , "* c5"
        , "* c2"
        , "* c1"
        , "* c0"
        ]

    it "detects empty rebases" $ do
      (history, branches, _tagRefs, _tagAnns) <- withTestEnv' $ \ shas runLoop _git -> do
        let [_c0, _c1, _c2, _c3, _c3', _c4, _c5, c6, _c7, c7f, _c8] = shas
            pr6 = PullRequestId 6
            pr8 = PullRequestId 8 -- PR#8 is built with 3 commits on top of PR#6
            branch6 = Branch "intro"
            branch8 = Branch "fixup" -- a fork from intro with all its changes
            baseBranch = masterBranch

        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr6 branch6 baseBranch c6 "Add Leon test results" "deckard" Nothing,
            Logic.PullRequestOpened pr8 branch8 baseBranch c7f "Update Leon data" "deckard" Nothing,
            Logic.CommentAdded pr8 "rachael" "@bot merge"
          ]

        Project.unfailedIntegratedPullRequests state `shouldBe` [pr8]

        let [rebasedSha] = integrationShas state

        -- merging PR#8 implies all changes from PR#6 are already in master

        state' <- runLoop state
          [
            Logic.BuildStatusChanged rebasedSha "default" BuildSucceeded,
            Logic.PullRequestCommitChanged (PullRequestId 8) rebasedSha,
            Logic.CommentAdded pr6 "rachael" "@bot merge"
          ]

        Project.unfailedIntegratedPullRequests state' `shouldBe` []

        let Just pullRequest' = Project.lookupPullRequest pr6 state'
        Project.integrationStatus pullRequest' `shouldBe`
          Project.Conflicted baseBranch Git.EmptyRebase
        -- since all changes are already into master, the merge is aborted.
        -- Users are notified and they should manually close the PR.

      history `shouldBe`
        [ "*   Merge #8"
        , "|\\"
        , "| * c8"
        , "| * c7"
        , "| * c6"
        , "| * c5"
        , "|/"
        , "* c3"
        , "* c2"
        , "* c1"
        , "* c0"
        ]
      branches `shouldMatchList`
        fmap Branch ["ahead", "intro", "master", "alternative", "fixup", "unused"]
