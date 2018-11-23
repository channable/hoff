-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Git
(
  Branch (..),
  CloneResult (..),
  GitOperation,
  PushResult (..),
  RemoteUrl (..),
  Sha (..),
  callGit,
  clone,
  cloneLocal,
  doesGitDirectoryExist,
  fetchBranch,
  forcePush,
  push,
  pushDelete,
  rebase,
  runGit,
  runBuild,
  tryIntegrate,
  unqualify,
)
where

import Control.Monad (mzero, when)
import Control.Monad.Free (Free (Free, Pure), liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, logWarnN)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, Value (String))
import Data.Hashable (Hashable)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.Format.Params (Params)
import Data.Text.Lazy (toStrict)
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process.Text (readCreateProcessWithExitCode)

import qualified Data.Text as Text
import qualified Data.Text.Format as Text
import qualified System.Process as Process

import Configuration (UserConfiguration)

import qualified Configuration as Config

-- Conversion function because of Haskell string type madness. This is just
-- Text.format, but returning a strict Text instead of a lazy one.
format :: Params ps => Text.Format -> ps -> Text
format formatString params = toStrict $ Text.format formatString params

-- A branch is identified by its name.
newtype Branch = Branch Text deriving (Eq)

-- A commit hash is stored as its hexadecimal representation.
newtype Sha = Sha Text deriving (Eq, FromJSONKey, Ord, Generic, Hashable, ToJSONKey)

newtype RemoteUrl = RemoteUrl Text deriving (Eq)

instance Show Branch where
  show (Branch branch) = Text.unpack branch

instance Show Sha where
  show (Sha sha) = Text.unpack sha

instance Show RemoteUrl where
  show (RemoteUrl url) = Text.unpack url

instance FromJSON Branch where
  parseJSON (String str) = return (Branch str)
  parseJSON _            = mzero

instance ToJSON Branch where
  toJSON (Branch str) = String str

instance FromJSON Sha where
  parseJSON (String str) = return (Sha str)
  parseJSON _            = mzero

instance ToJSON Sha where
  toJSON (Sha str) = String str

instance FromJSON RemoteUrl where
  parseJSON (String url) = pure (RemoteUrl url)
  parseJSON _            = mzero

instance ToJSON RemoteUrl where
  toJSON (RemoteUrl url) = String url

data PushResult
  = PushOk
  | PushRejected
  deriving (Eq, Show)

data CloneResult
  = CloneOk
  | CloneFailed
  deriving (Eq, Show)

data GitOperationFree a
  = FetchBranch Branch a
  | ForcePush Sha Branch a
  | PushDelete Branch a
  | Push Sha Branch (PushResult -> a)
  | Rebase Sha Branch (Maybe Sha -> a)
  | Clone RemoteUrl (CloneResult -> a)
  | CloneLocal Sha Branch FilePath (CloneResult -> a)
  | DoesGitDirectoryExist (Bool -> a)
    -- TODO: Should be in a different free monad, but meh.
    -- Also, should not take config, should read it from the interpreter
    -- instead.
  | RunBuild Text Text FilePath Sha FilePath FilePath a
  deriving (Functor)

type GitOperation = Free GitOperationFree

-- Strip the refs/heads/ or refs/tags/ prefix.
unqualify :: Branch -> Branch
unqualify (Branch refname) = case refname of
  (Text.stripPrefix "refs/heads/" -> Just suff) -> Branch suff
  (Text.stripPrefix "refs/tags/" -> Just suff) -> Branch suff
  _ -> Branch refname

fetchBranch :: Branch -> GitOperation ()
fetchBranch remoteBranch = liftF $ FetchBranch remoteBranch ()

forcePush :: Sha -> Branch -> GitOperation ()
forcePush sha remoteBranch = liftF $ ForcePush sha remoteBranch ()

push :: Sha -> Branch -> GitOperation PushResult
push sha remoteBranch = liftF $ Push sha remoteBranch id

pushDelete :: Branch -> GitOperation ()
pushDelete remoteBranch = liftF $ PushDelete remoteBranch ()

rebase :: Sha -> Branch -> GitOperation (Maybe Sha)
rebase sha ontoBranch = liftF $ Rebase sha ontoBranch id

clone :: RemoteUrl -> GitOperation CloneResult
clone url = liftF $ Clone url id

cloneLocal :: Sha -> Branch -> FilePath -> GitOperation CloneResult
cloneLocal targetHead targetBranch targetDir = liftF $ CloneLocal targetHead targetBranch targetDir id

doesGitDirectoryExist :: GitOperation Bool
doesGitDirectoryExist = liftF $ DoesGitDirectoryExist id

runBuild :: Text -> Text -> FilePath -> Sha -> FilePath -> FilePath -> GitOperation ()
runBuild owner repo logFile sha buildDir buildBin = liftF $ RunBuild owner repo logFile sha buildDir buildBin ()

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

-- Invokes Git with the given arguments. Returns its output on success, or the
-- exit code and stderr on error.
callGit
  :: (MonadIO m, MonadLogger m)
  => UserConfiguration
  -> [String]
  -> m (Either (ExitCode, Text) Text)
callGit userConfig args = do
  currentEnv <- liftIO getEnvironment
  let
    commandText  = Text.concat $ intersperse " " $ fmap Text.pack args
    logMessage   = Text.append "executing git " commandText
    stdinContent = ""
    process = (Process.proc "git" args) {
      -- Prepend GIT_EDITOR to the environment and set it to "true".
      -- For an interactive rebase, this ensures that we close the editor
      -- immediately. Note that sometimes true is /usr/bin/true and sometimes
      -- it is /bin/true, so we have use /usr/bin/env to locate it, assuming
      -- that env is in a consistent location.  Also use a custom ssh command,
      -- in order to select the location of the secret key. Finally, tell Git to
      -- not prompt for things such as passphrases, because there is no
      -- interactive terminal.
      Process.env = Just
        $ ("GIT_EDITOR", "/usr/bin/env true")
        : ("GIT_SSH_COMMAND", "/usr/bin/ssh -F " ++ (Config.sshConfigFile userConfig))
        : ("GIT_TERMINAL_PROMPT", "0")
        : currentEnv
    }
    runProcess = readCreateProcessWithExitCode process stdinContent
  logInfoN logMessage
  (exitCode, output, errors) <- liftIO runProcess
  if exitCode == ExitSuccess
    then return $ Right output
    else return $ Left (exitCode, errors)

-- Interpreter for the GitOperation free monad that starts Git processes and
-- parses its output.
runGit
  :: (MonadIO m, MonadLogger m)
  => UserConfiguration
  -> FilePath
  -> GitOperation a
  -> m a
runGit userConfig repoDir operation =
  let
    -- Pass the -C /path/to/checkout option to Git, to run operations in the
    -- repository without having to change the working directory.
    callGitInRepo args = callGit userConfig $ ["-C", repoDir] ++ args
    continueWith       = runGit userConfig repoDir
  in case operation of
    Pure result -> return result

    Free (FetchBranch branch cont) -> do
      result <- callGitInRepo ["fetch", "origin", show branch]
      case result of
        Left  _ -> logWarnN "warning: git fetch failed"
        Right _ -> return ()
      continueWith cont

    Free (ForcePush sha branch cont) -> do
      -- TODO: Make Sha and Branch constructors sanitize data, otherwise this
      -- could run unintended Git commands.
      -- Note: the remote branch is prefixed with 'refs/heads/' to specify the
      -- branch unambiguously. This will make Git create the branch if it does
      -- not exist.
      result <- callGitInRepo ["push", "--force", "origin", (show sha) ++ ":refs/heads/" ++ (show branch)]
      case result of
        Left  _ -> logWarnN "warning: git push --force failed"
        Right _ -> return ()
      continueWith cont

    Free (Push sha branch cont) -> do
      result <- callGitInRepo ["push", "origin", (show sha) ++ ":refs/heads/" ++ (show branch)]
      let pushResult = case result of
            Left  _ -> PushRejected
            Right _ -> PushOk
      when (pushResult == PushRejected) $ logInfoN "push was rejected"
      continueWith $ cont pushResult

    Free (PushDelete branch cont) -> do
      result <- callGitInRepo ["push", "origin", ":refs/heads/" ++ (show branch)]
      case result of
        Left  _ -> logWarnN "warning: failed to delete remote branch"
        Right _ -> pure ()
      continueWith cont

    Free (Rebase sha branch cont) -> do
      -- Do an interactive rebase with editor set to /usr/bin/true, so we just
      -- accept the default action, which is effectively a non-interactive rebase.
      -- The interactive rebase is required for --autosquash, which automatically
      -- puts !fixup and !squash commits in the right place.
      result <- callGitInRepo
        [ "rebase", "--interactive", "--autosquash"
        , "origin/" ++ (show branch), show sha
        ]
      case result of
        Left (code, message) -> do
          -- Rebase failed, call the continuation with no rebased sha, but first
          -- abort the rebase.
          -- TODO: Don't spam the log with these, a failed rebase is expected.
          logInfoN $ format "git rebase failed with code {}: {}" (show code, message)
          abortResult <- callGitInRepo ["rebase", "--abort"]
          when (isLeft abortResult) $ logWarnN "warning: git rebase --abort failed"
          continueWith $ cont Nothing
        Right _ -> do
          revResult <- callGitInRepo ["rev-parse", "@"]
          case revResult of
            Left  _   -> do
              logWarnN "warning: git rev-parse failed"
              continueWith $ cont Nothing
            Right newSha -> continueWith $ cont $ Just $ Sha $ Text.strip newSha

    Free (Clone url cont) -> do
      result <- callGit userConfig
        -- Pass some config flags, that get applied as the repository is
        -- initialized, before the clone. This means we can enable fsckObjects
        -- and have the clone be checked.
        -- TODO: Recursive clone?
        [ "clone"
        , "--config", "transfer.fsckObjects=true"
        , "--config", "user.name=" ++ (Text.unpack $ Config.name userConfig)
        , "--config", "user.email=" ++ (Text.unpack $ Config.email userConfig)
        , show url, repoDir
        ]
      case result of
        Left (code, message) -> do
          logWarnN $ format "git clone failed with code {}: {}" (show code, message)
          continueWith (cont CloneFailed)
        Right _ -> do
          logInfoN $ format "cloned {} succesfully" [show url]
          continueWith (cont CloneOk)

    Free (CloneLocal sha branch targetPath cont) -> do
      result <- callGit userConfig
        -- Clone with --local and --reference to make the clone share its Git
        -- objects with the original repository. This makes the checkout use
        -- very little additional space. Check out the desired commit afterwards.
        ["clone" , "--local", "--reference", repoDir, "--", repoDir, targetPath]
      case result of
        Left (code, message) -> do
          logWarnN $ format "git clone failed with code {}: {}" (show code, message)
          continueWith (cont CloneFailed)
        Right _ -> do
          refResult <- callGit userConfig ["-C", targetPath, "update-ref", show branch, show sha]
          case refResult of
            Left (code, message) -> do
              logWarnN $ format "git update-ref failed with code {}: {}" (show code, message)
              continueWith (cont CloneFailed)
            Right _ -> do
              checkoutResult <- callGit userConfig ["-C", targetPath, "checkout", show $ unqualify branch]
              case checkoutResult of
                Left (code, message) -> do
                  logWarnN $ format "git checkout failed with code {}: {}" (show code, message)
                  continueWith (cont CloneFailed)
                Right _ -> do
                  logInfoN $ format "local checkout {} created succesfully" [targetPath]
                  continueWith (cont CloneOk)

    Free (DoesGitDirectoryExist cont) -> do
      exists <- liftIO $ doesDirectoryExist (repoDir </> ".git")
      continueWith (cont exists)

    Free (RunBuild owner repo logFile sha buildDir buildBin cont) ->
      let
        args =
          [ "--owner", show owner
          , "--repo", show repo
          , "--sha", show sha
          , "--log-file", logFile
          ]
        argStr = concat $ intersperse " " args
        proc = (Process.proc buildBin args) { Process.cwd = Just buildDir }
      in do
        logInfoN $ format "Starting build command in {} with args {}." (buildDir, argStr)
        _ <- liftIO $ Process.createProcess proc
        continueWith cont

-- Fetches the target branch, rebases the candidate on top of the target branch,
-- and if that was successfull, force-pushses the resulting commits to the test
-- branch.
tryIntegrate :: Branch -> Sha -> Branch -> Branch -> GitOperation (Maybe Sha)
tryIntegrate candidateRef candidateSha targetBranch testBranch = do
  -- Fetch the ref for the target commit that needs to be rebased, we might not
  -- have it yet. Although Git supports fetching single commits, GitHub does
  -- not, so we fetch the /refs/pull/:id/head ref that GitHub creates for every
  -- pull request, and that should point to the candidate commit.
  fetchBranch candidateRef
  -- Make sure the target branch is up to date. (If later -- after the test
  -- results come in, and we want to push -- it turns out that the target branch
  -- has new commits, then we just restart the cycle.)
  fetchBranch targetBranch
  -- Rebase the candidate commits onto the target branch.
  rebaseResult <- rebase candidateSha targetBranch
  case rebaseResult of
    -- If the rebase succeeded, then this is our new integration candidate.
    -- Push it to the remote integration branch to trigger a build.
    Just sha -> forcePush sha testBranch >> return (Just sha)
    Nothing  -> return Nothing
