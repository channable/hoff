-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Git
(
  Branch (..),
  CloneResult (..),
  GitOperation,
  GitOperationFree,
  PushResult (..),
  RemoteUrl (..),
  Sha (..),
  callGit,
  clone,
  doesGitDirectoryExist,
  fetchBranch,
  forcePush,
  push,
  rebase,
  runGit,
  runGitReadOnly,
  tryIntegrate,
)
where

import Control.Monad (mzero, when)
import Control.Monad.Free (Free, liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, logWarnN)
import Data.Aeson
import Data.List (intersperse)
import Data.Text (Text)
import System.Directory (doesDirectoryExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process.Text (readCreateProcessWithExitCode)

import qualified Data.Text as Text
import qualified System.Process as Process

import Configuration (UserConfiguration)
import Format (format)

import qualified Configuration as Config

-- A branch is identified by its name.
newtype Branch = Branch Text deriving (Eq)

-- A commit hash is stored as its hexadecimal representation.
newtype Sha = Sha Text deriving (Eq)

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
  | Push Sha Branch (PushResult -> a)
  | Rebase Sha Branch (Maybe Sha -> a)
  | Merge Sha Text (Maybe Sha -> a)
  | Checkout Branch (Maybe Sha -> a)
  | Clone RemoteUrl (CloneResult -> a)
  | GetParent Sha (Maybe Sha -> a)
  | DoesGitDirectoryExist (Bool -> a)
  deriving (Functor)

type GitOperation = Free GitOperationFree

fetchBranch :: Branch -> GitOperation ()
fetchBranch remoteBranch = liftF $ FetchBranch remoteBranch ()

forcePush :: Sha -> Branch -> GitOperation ()
forcePush sha remoteBranch = liftF $ ForcePush sha remoteBranch ()

push :: Sha -> Branch -> GitOperation PushResult
push sha remoteBranch = liftF $ Push sha remoteBranch id

rebase :: Sha -> Branch -> GitOperation (Maybe Sha)
rebase sha ontoBranch = liftF $ Rebase sha ontoBranch id

merge :: Sha -> Text -> GitOperation (Maybe Sha)
merge sha message = liftF $ Merge sha message id

-- Check out the commit that origin/<branch> points to. So we end up in a
-- detached HEAD state. Returns the sha of the checked out commit.
checkout :: Branch -> GitOperation (Maybe Sha)
checkout branch = liftF $ Checkout branch id

-- Return the parent of the given commit.
getParent :: Sha -> GitOperation (Maybe Sha)
getParent sha = liftF $ GetParent sha id

clone :: RemoteUrl -> GitOperation CloneResult
clone url = liftF $ Clone url id

doesGitDirectoryExist :: GitOperation Bool
doesGitDirectoryExist = liftF $ DoesGitDirectoryExist id

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
  :: forall m a
   . MonadIO m
  => MonadLogger m
  => UserConfiguration
  -> FilePath
  -> GitOperationFree a
  -> m a
runGit userConfig repoDir operation =
  let
    -- Pass the -C /path/to/checkout option to Git, to run operations in the
    -- repository without having to change the working directory.
    callGitInRepo args = callGit userConfig $ ["-C", repoDir] ++ args

    getHead :: m (Maybe Sha)
    getHead = do
      revResult <- callGitInRepo ["rev-parse", "@"]
      case revResult of
        Left  _   -> do
          logWarnN "warning: git rev-parse failed"
          pure Nothing
        Right newSha ->
          pure $ Just $ Sha $ Text.strip newSha

  in case operation of
    FetchBranch branch cont -> do
      result <- callGitInRepo ["fetch", "origin", show branch]
      case result of
        Left  _ -> logWarnN "warning: git fetch failed"
        Right _ -> return ()
      pure cont

    ForcePush sha branch cont -> do
      -- TODO: Make Sha and Branch constructors sanitize data, otherwise this
      -- could run unintended Git commands.
      -- Note: the remote branch is prefixed with 'refs/heads/' to specify the
      -- branch unambiguously. This will make Git create the branch if it does
      -- not exist.
      result <- callGitInRepo ["push", "--force-with-lease", "origin", (show sha) ++ ":refs/heads/" ++ (show branch)]
      case result of
        Left  _ -> logWarnN "warning: git push --force-with-lease failed"
        Right _ -> return ()
      pure cont

    Push sha branch cont -> do
      result <- callGitInRepo ["push", "origin", (show sha) ++ ":refs/heads/" ++ (show branch)]
      let pushResult = case result of
            Left  _ -> PushRejected
            Right _ -> PushOk
      when (pushResult == PushRejected) $ logInfoN "push was rejected"
      pure $ cont pushResult

    Rebase sha branch cont -> do
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
          pure $ cont Nothing
        Right _ -> do
          rev <- getHead
          pure $ cont rev

    Merge sha message cont -> do
      result <- callGitInRepo
        [ "merge"
        , "--no-ff"
        , "-m"
        , Text.unpack message
        , show sha
        ]
      case result of
        Left (code, output) -> do
          -- Merge failed, call the continuation with no rebased sha, but first
          -- abort the merge, if any.
          logInfoN $ format "git merge failed with code {}: {}" (show code, output)
          abortResult <- callGitInRepo ["merge", "--abort"]
          when (isLeft abortResult) $ logWarnN "git merge --abort failed"
          pure $ cont Nothing
        Right _ -> do
          rev <- getHead
          pure $ cont rev

    Checkout branch cont -> do
      branchRev <- callGitInRepo ["rev-parse", "origin/" ++ (show branch)]
      case branchRev of
        Left _ -> do
          logWarnN "git rev-parse failed"
          pure $ cont Nothing
        Right sha -> do
          let strippedSha = Text.strip sha
          result <- callGitInRepo ["checkout", Text.unpack strippedSha]
          when (isLeft result) $ logWarnN "git checkout failed"
          pure $ cont $ Just $ Sha strippedSha

    GetParent sha cont -> do
      parentRev <- callGitInRepo ["rev-parse", (show sha) ++ "^"]
      case parentRev of
        Left _ -> do
          logWarnN "git rev-parse to get parent failed"
          pure $ cont Nothing
        Right parentSha ->
          pure $ cont $ Just $ Sha $ Text.strip parentSha

    Clone url cont -> do
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
          pure $ cont CloneFailed
        Right _ -> do
          logInfoN $ format "cloned {} succesfully" [show url]
          pure $ cont CloneOk

    DoesGitDirectoryExist cont -> do
      exists <- liftIO $ doesDirectoryExist (repoDir </> ".git")
      pure $ cont exists

-- Interpreter that runs only Git operations that have no side effects on the
-- remote; it does not push.
runGitReadOnly
  :: forall m a
   . MonadIO m
  => MonadLogger m
  => UserConfiguration
  -> FilePath
  -> GitOperationFree a
  -> m a
runGitReadOnly userConfig repoDir operation =
  let
    unsafeResult = runGit userConfig repoDir operation
  in
    case operation of
      -- These operations only operate locally, or only perform reads from the
      -- remote, so they are safe to execute.
      FetchBranch {} -> unsafeResult
      Rebase {} -> unsafeResult
      Merge {} -> unsafeResult
      Checkout {} -> unsafeResult
      Clone {} -> unsafeResult
      GetParent {} -> unsafeResult
      DoesGitDirectoryExist {} -> unsafeResult

      -- These operations mutate the remote, so we don't execute them in
      -- read-only mode.
      ForcePush (Sha sha) (Branch branch) cont -> do
        logInfoN $ Text.concat ["Would have force-pushed ", sha, " to ", branch]
        pure $ cont
      Push (Sha sha) (Branch branch) cont -> do
        logInfoN $ Text.concat ["Would have pushed ", sha, " to ", branch]
        pure $ cont PushRejected

-- Fetches the target branch, rebases the candidate on top of the target branch,
-- and if that was successfull, force-pushses the resulting commits to the test
-- branch.
tryIntegrate :: Text -> Branch -> Sha -> Branch -> Branch -> GitOperation (Maybe Sha)
tryIntegrate message candidateRef candidateSha targetBranch testBranch = do
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
    Nothing  -> pure Nothing
    Just sha -> do
      -- After the rebase, we also do a (non-fast-forward) merge, to clarify
      -- that this is a single unit of change; a way to fake "chapters" in the
      -- history. We only do this if there is more than one commit to integrate.
      -- If not (when the current master is the parent of the proposed commit),
      -- then we just take that commit as-is.
      targetBranchSha <- checkout targetBranch
      parentSha       <- getParent sha
      newTip <- case parentSha of
        Nothing -> pure $ Just sha
        parent | parent == targetBranchSha -> pure $ Just sha
        _moreThanOneCommitBehind           -> merge sha message

      -- If both the rebase, and the (potential) merge went well, push it to the
      -- testing branch so CI will build it.
      case newTip of
        Just tipSha -> forcePush tipSha testBranch
        Nothing     -> pure ()

      pure newTip
