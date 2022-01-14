-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Git
(
  Branch (..),
  BaseBranch (..),
  CloneResult (..),
  GitOperation,
  GitOperationFree,
  GitIntegrationFailure (..),
  PushResult (..),
  RefSpec(..),
  RemoteBranch(..),
  RemoteUrl (..),
  Sha (..),
  SomeRefSpec( .. ),
  TagMessage (..),
  TagName (..),
  TagResult (..),
  callGit,
  clone,
  deleteTag,
  doesGitDirectoryExist,
  fetchBranch,
  fetchBranchWithTags,
  forcePush,
  lastTag,
  listRecentlyTouchedFiles,
  shortlog,
  push,
  pushAtomic,
  rebase,
  runGit,
  runGitReadOnly,
  tag,
  tag',
  tryIntegrate,
)
where

import Control.Monad (mzero, when)
import Control.Monad.Free (Free, liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, logWarnN)
import Data.Aeson
import Data.Either (isLeft)
import Data.List (intersperse)
import Data.Text (Text, splitOn, unpack)
import System.Directory (doesDirectoryExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process.Text (readCreateProcessWithExitCode)

import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified System.Process as Process

import Configuration (UserConfiguration)
import Format (format)

import qualified Configuration as Config
import GitHub.Internal.Prelude (Generic)

-- | A branch is identified by its name.
newtype Branch = Branch Text deriving newtype (Show, Eq)

-- | A type to represent the base branch of a PR.
newtype BaseBranch = BaseBranch Text deriving newtype (Show, Eq)

-- | A branch identified by its name, pointing at origin.
newtype RemoteBranch = RemoteBranch Text deriving newtype (Show, Eq)

localBranch :: RemoteBranch -> Branch
localBranch (RemoteBranch name) = Branch name

-- | A commit hash is stored as its hexadecimal representation.
newtype Sha = Sha Text deriving newtype (Show, Eq)

newtype RemoteUrl = RemoteUrl Text deriving newtype (Show, Eq)

newtype TagMessage = TagMessage Text deriving newtype (Show, Eq)

newtype TagName = TagName Text deriving newtype (Show, Eq)

class RefSpec a where
  refSpec :: a -> String

instance RefSpec Branch where
  refSpec (Branch name) = Text.unpack name

instance RefSpec BaseBranch where
  refSpec (BaseBranch name) = Text.unpack name

instance RefSpec RemoteBranch where
  refSpec (RemoteBranch name) = "origin/" ++ Text.unpack name

instance RefSpec Sha where
  refSpec (Sha sha) = Text.unpack $ Text.strip sha

instance RefSpec (Sha, Branch) where
  refSpec (sha, remote) = refSpec sha ++ ":refs/heads/" ++ refSpec remote

instance RefSpec (Sha, BaseBranch) where
  refSpec (sha, remote) = refSpec sha ++ ":refs/heads/" ++ refSpec remote

instance RefSpec TagName where
  refSpec (TagName name) = Text.unpack name

data SomeRefSpec = forall a . RefSpec a => AsRefSpec a

instance RefSpec SomeRefSpec where
  refSpec (AsRefSpec a) = refSpec a

instance FromJSON Branch where
  parseJSON (String str) = return (Branch str)
  parseJSON _            = mzero

instance ToJSON Branch where
  toJSON (Branch str) = String str

instance FromJSON BaseBranch where
  parseJSON (String str) = return (BaseBranch str)
  parseJSON _            = mzero

instance ToJSON BaseBranch where
  toJSON (BaseBranch str) = String str

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
  deriving stock (Eq, Show)

data CloneResult
  = CloneOk
  | CloneFailed
  deriving stock (Eq, Show)

data TagResult
  = TagOk TagName
  | TagFailed Text
  deriving stock (Eq, Show)

data FetchWithTags = WithTags | NoTags
  deriving stock (Eq, Show)

data GitIntegrationFailure
  = MergeFailed
  | RebaseFailed
  | WrongFixups
  deriving (Show, Eq, Generic)

instance FromJSON GitIntegrationFailure

instance ToJSON GitIntegrationFailure where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

data GitOperationFree a
  = FetchBranch Branch FetchWithTags a
  | ForcePush Sha Branch a
  | Push Sha Branch (PushResult -> a)
  | PushAtomic [SomeRefSpec] (PushResult -> a)
  | Rebase Sha RemoteBranch (Maybe Sha -> a)
  | Merge Sha Text (Maybe Sha -> a)
  | Checkout RemoteBranch (Maybe Sha -> a)
  | Clone RemoteUrl (CloneResult -> a)
  | GetParent Sha (Maybe Sha -> a)
  | DoesGitDirectoryExist (Bool -> a)
  | LastTag Sha (Maybe Text -> a)
  | ShortLog SomeRefSpec SomeRefSpec (Maybe Text -> a)
  | Tag Sha TagName TagMessage (TagResult -> a)
  | DeleteTag TagName a
  | CheckOrphanFixups Sha RemoteBranch (Bool -> a)
  | ListRecentlyTouchedFiles SomeRefSpec SomeRefSpec FilePath (Maybe [FilePath] -> a)
  deriving (Functor)

type GitOperation = Free GitOperationFree

fetchBranch :: Branch -> GitOperation ()
fetchBranch remoteBranch = liftF $ FetchBranch remoteBranch NoTags ()

fetchBranchWithTags :: Branch -> GitOperation ()
fetchBranchWithTags remoteBranch = liftF $ FetchBranch remoteBranch WithTags ()

forcePush :: Sha -> Branch -> GitOperation ()
forcePush sha remoteBranch = liftF $ ForcePush sha remoteBranch ()

push :: Sha -> Branch -> GitOperation PushResult
push sha remoteBranch = liftF $ Push sha remoteBranch id

pushAtomic :: [SomeRefSpec] -> GitOperation PushResult
pushAtomic refs = liftF $ PushAtomic refs id

rebase :: Sha -> RemoteBranch -> GitOperation (Maybe Sha)
rebase sha ontoBranch = liftF $ Rebase sha ontoBranch id

merge :: Sha -> Text -> GitOperation (Maybe Sha)
merge sha message = liftF $ Merge sha message id

-- Check out the commit that origin/<branch> points to. So we end up in a
-- detached HEAD state. Returns the sha of the checked out commit.
checkout :: RemoteBranch -> GitOperation (Maybe Sha)
checkout branch = liftF $ Checkout branch id

-- Return the parent of the given commit.
getParent :: Sha -> GitOperation (Maybe Sha)
getParent sha = liftF $ GetParent sha id

clone :: RemoteUrl -> GitOperation CloneResult
clone url = liftF $ Clone url id

doesGitDirectoryExist :: GitOperation Bool
doesGitDirectoryExist = liftF $ DoesGitDirectoryExist id

lastTag :: Sha -> GitOperation (Maybe TagName)
lastTag sha = liftF $ LastTag sha (TagName . Text.strip <$>)

shortlog :: SomeRefSpec -> SomeRefSpec -> GitOperation (Maybe Text)
shortlog refStart refEnd = liftF $ ShortLog refStart refEnd id

tag :: Sha -> TagName -> TagMessage -> GitOperation TagResult
tag sha name message = liftF $ Tag sha name message id

tag' :: Sha -> TagName -> GitOperation TagResult
tag' sha t@(TagName name) = tag sha t (TagMessage name)

deleteTag :: TagName -> GitOperation ()
deleteTag t = liftF $ DeleteTag t ()

checkOrphanFixups :: Sha -> RemoteBranch -> GitOperation Bool
checkOrphanFixups sha branch = liftF $ CheckOrphanFixups sha branch id

listRecentlyTouchedFiles :: SomeRefSpec -> SomeRefSpec -> FilePath -> GitOperation (Maybe [FilePath])
listRecentlyTouchedFiles compareRev compareTo directory = liftF $ ListRecentlyTouchedFiles compareRev compareTo directory id

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
          pure $ Just $ Sha $ Text.stripEnd newSha

  in case operation of
    FetchBranch branch withTags cont -> do
      result <- callGitInRepo $ case withTags of
        WithTags -> ["fetch", "--tags", "origin", refSpec branch]
        NoTags   -> ["fetch", "origin", refSpec branch]
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
      result <- callGitInRepo ["push", "--force", "origin", refSpec (sha, branch)]
      case result of
        Left  _ -> logWarnN "warning: git push --force failed"
        Right _ -> return ()
      pure cont

    Push sha branch cont -> do
      result <- callGitInRepo ["push", "origin", refSpec (sha, branch)]
      let pushResult = case result of
            Left  _ -> PushRejected
            Right _ -> PushOk
      when (pushResult == PushRejected) $ logInfoN "push was rejected"
      pure $ cont pushResult

    Rebase sha remoteBranch cont -> do
      -- Do an interactive rebase with editor set to /usr/bin/true, so we just
      -- accept the default action, which is effectively a non-interactive rebase.
      -- The interactive rebase is required for --autosquash, which automatically
      -- puts !fixup and !squash commits in the right place.
      result <- callGitInRepo
        [ "rebase", "--interactive", "--autosquash"
        ,  refSpec remoteBranch, refSpec sha
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
        Right _ -> cont <$> getHead

    Merge sha message cont -> do
      result <- callGitInRepo
        [ "merge"
        , "--no-ff"
        , "-m"
        , Text.unpack message
        , refSpec sha
        ]
      case result of
        Left (code, output) -> do
          -- Merge failed, call the continuation with no rebased sha, but first
          -- abort the merge, if any.
          logInfoN $ format "git merge failed with code {}: {}" (show code, output)
          abortResult <- callGitInRepo ["merge", "--abort"]
          when (isLeft abortResult) $ logWarnN "git merge --abort failed"
          pure $ cont Nothing
        Right _ -> cont <$> getHead

    Checkout remoteBranch cont -> do
      branchRev <- callGitInRepo ["rev-parse", refSpec remoteBranch]
      case branchRev of
        Left _ -> do
          logWarnN "git rev-parse failed"
          pure $ cont Nothing
        Right parsed -> do
          let sha = Sha $ Text.stripEnd parsed
          result <- callGitInRepo ["checkout", refSpec sha]
          when (isLeft result) $ logWarnN "git checkout failed"
          pure $ cont $ Just sha

    GetParent sha cont -> do
      parentRev <- callGitInRepo ["rev-parse", refSpec sha ++ "^"]
      case parentRev of
        Left _ -> do
          logWarnN "git rev-parse to get parent failed"
          pure $ cont Nothing
        Right parentSha ->
          pure $ cont $ Just $ Sha $ Text.stripEnd parentSha

    Clone (RemoteUrl url) cont -> do
      result <- callGit userConfig
        -- Pass some config flags, that get applied as the repository is
        -- initialized, before the clone. This means we can enable fsckObjects
        -- and have the clone be checked.
        -- TODO: Recursive clone?
        [ "clone"
        , "--config", "transfer.fsckObjects=true"
        , "--config", "user.name=" ++ (Text.unpack $ Config.name userConfig)
        , "--config", "user.email=" ++ (Text.unpack $ Config.email userConfig)
        , Text.unpack url
        , repoDir
        ]
      case result of
        Left (code, message) -> do
          logWarnN $ format "git clone failed with code {}: {}" (show code, message)
          pure $ cont CloneFailed
        Right _ -> do
          logInfoN $ format "cloned {} successfully" [url]
          pure $ cont CloneOk

    DoesGitDirectoryExist cont -> do
      exists <- liftIO $ doesDirectoryExist (repoDir </> ".git")
      pure $ cont exists

    PushAtomic refs cont -> do
      result <- callGitInRepo $ ["push", "--atomic", "origin"] ++ map refSpec refs
      let pushResult = case result of
            Left  _ -> PushRejected
            Right _ -> PushOk
      when (pushResult == PushRejected) $ logInfoN "atomic push was rejected"
      pure $ cont pushResult
    LastTag sha cont -> do
      result <- callGitInRepo ["describe", "--abbrev=0", "--tags", refSpec sha]
      pure $ cont $ either (const Nothing) Just result

    ShortLog shaStart shaEnd cont -> do
      result <- callGitInRepo ["shortlog", refSpec shaStart <> ".." <> refSpec shaEnd]
      case result of
        Left (code, message) -> do
          logWarnN $ format "git shortlog failed with code {}: {}" (show code, message)
          pure $ cont Nothing
        Right changelog -> do
          pure $ cont $ Just changelog

    Tag sha t (TagMessage m) cont -> do
      result <- callGitInRepo ["tag", "-a", refSpec t, "-m", Text.unpack m, refSpec sha]
      case result of
        Left (code, message) -> do
          logWarnN $ format "git tag failed with code {}: {}" (show code, message)
          pure $ cont $ TagFailed message
        Right _ -> do
          logInfoN $ format "tagged {} with {}" [show sha, show t]
          pure $ cont $ TagOk t

    DeleteTag t cont -> cont <$ callGitInRepo ["tag", "-d", refSpec t]

    CheckOrphanFixups sha branch cont -> do
      result <- let branch' = refSpec branch
                    sha' = refSpec sha
                in callGitInRepo ["log", Text.unpack $ format "{}..{}" [branch',sha'], "--format=%s"]
      case result of
        Left (code, message) -> do
          logWarnN $ format "git log failed with code {}: {}" (show code, message)
          pure $ cont False
        Right logResponse -> do
          let anyOrphanFixups = any (\x -> "fixup!" `Text.isPrefixOf` x) $ Text.lines logResponse
          when anyOrphanFixups $
            logWarnN "there is one ore more fixup commits not belonging to any other commit"
          pure $ cont anyOrphanFixups

    ListRecentlyTouchedFiles compareRev compareTo directory cont -> do
      let range = refSpec compareRev <> ".." <> refSpec compareTo
      result <- callGitInRepo [ "diff", "--no-renames", "--name-only", "-z", range, "--", directory]
      case result of
        Left (code, message) -> do
          logWarnN $ format "git log failed with code {}: {}" (show code, message)
          pure $ cont Nothing
        Right fileText ->
          pure $ cont (Just (map unpack $ splitOn "\0" fileText))


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
      LastTag {} -> unsafeResult
      ShortLog {} -> unsafeResult
      Tag {} -> unsafeResult
      DeleteTag {} -> unsafeResult
      CheckOrphanFixups {} -> unsafeResult
      ListRecentlyTouchedFiles {} -> unsafeResult

      -- These operations mutate the remote, so we don't execute them in
      -- read-only mode.
      ForcePush (Sha sha) (Branch branch) cont -> do
        logInfoN $ Text.concat ["Would have force-pushed ", sha, " to ", branch]
        pure cont
      Push (Sha sha) (Branch branch) cont -> do
        logInfoN $ Text.concat ["Would have pushed ", sha, " to ", branch]
        pure $ cont PushRejected
      PushAtomic refs cont -> do
        logInfoN
          $ "Would have pushed atomically the following refs: "
          <> Text.intercalate "," (map (Text.pack . refSpec) refs)
        pure $ cont PushRejected

-- Fetches the target branch, rebases the candidate on top of the target branch,
-- and if that was successful, force-pushes the resulting commits to the test
-- branch.
tryIntegrate :: Text -> Branch -> Sha -> RemoteBranch -> Branch -> Bool -> GitOperation (Either GitIntegrationFailure Sha)
tryIntegrate message candidateRef candidateSha targetBranch testBranch alwaysAddMergeCommit = do
  -- Fetch the ref for the target commit that needs to be rebased, we might not
  -- have it yet. Although Git supports fetching single commits, GitHub does
  -- not, so we fetch the /refs/pull/:id/head ref that GitHub creates for every
  -- pull request, and that should point to the candidate commit.
  fetchBranch candidateRef
  -- Make sure the target branch is up to date. (If later -- after the test
  -- results come in, and we want to push -- it turns out that the target branch
  -- has new commits, then we just restart the cycle.)
  fetchBranchWithTags $ localBranch targetBranch
  -- Rebase the candidate commits onto the target branch.
  rebaseResult <- rebase candidateSha targetBranch
  case rebaseResult of
    -- If the rebase succeeded, then this is our new integration candidate.
    -- Push it to the remote integration branch to trigger a build.
    Nothing  -> pure $ Left RebaseFailed
    Just sha -> do
      -- Before merging, we check if there exist fixup commits that do not
      -- belong to any other commits. If there are no such fixups, we proceed
      -- with merging; otherwise we raise a warning and don't merge.
      -- After the rebase, we also do a (non-fast-forward) merge, to clarify
      -- that this is a single unit of change; a way to fake "chapters" in the
      -- history.
      -- We only do this if there is more than one commit to integrate (so the
      -- history stays linear for single-commit PRs) or the PR was approved for
      -- merge and deploy (because we can't record the "Auto-deploy: true"
      -- trailer otherwise).
      -- If not (i.e. the current master is the parent of the proposed commit
      -- and the approval type is not MergeAndDeploy) then we just take that
      -- commit as-is.
      hasOrphanFixups <- checkOrphanFixups sha targetBranch
      if hasOrphanFixups
        then pure $ Left WrongFixups
        else do
          targetBranchSha <- checkout targetBranch
          parentSha       <- getParent sha
          newTip <- case parentSha of
            Nothing -> pure $ Just sha
            parent
              | alwaysAddMergeCommit      -> merge sha message
              | parent == targetBranchSha -> pure $ Just sha
            _moreThanOneCommitBehind      -> merge sha message

          -- If both the rebase, and the (potential) merge went well, push it to the
          -- testing branch so CI will build it.
          case newTip of
            Just tipSha -> do
              forcePush tipSha testBranch
            Nothing     -> pure ()

          pure $ maybe (Left MergeFailed) Right newTip
