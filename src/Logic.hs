-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Logic
(
  Action,
  ActionFree (..),
  Event (..),
  EventQueue,
  IntegrationFailure (..),
  dequeueEvent,
  enqueueEvent,
  enqueueStopSignal,
  ensureCloned,
  handleEvent,
  newEventQueue,
  newStateVar,
  proceedUntilFixedPoint,
  readStateVar,
  runAction,
  tryIntegratePullRequest,
  updateStateVar,
)
where

import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Control.Concurrent.STM.TMVar (TMVar, newTMVarIO, readTMVar, swapTMVar)
import Control.Exception (assert)
import Control.Monad (foldM, unless, void, when)
import Control.Monad.Free (Free (..), foldFree, hoistFree, liftF)
import Control.Monad.STM (atomically)
import Data.Bifunctor (first)
import Data.Either.Extra (maybeToEither)
import Data.Functor.Sum (Sum (InL, InR))
import Data.IntSet (IntSet)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import GHC.Natural (Natural)

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Read as Text
import Data.Time (UTCTime, DayOfWeek (Friday), dayOfWeek, utctDay)

import Configuration (ProjectConfiguration, TriggerConfiguration, MergeWindowExemptionConfiguration)
import Format (format)
import Git (Branch (..), BaseBranch (..), GitOperation, GitOperationFree, PushResult (..),
            GitIntegrationFailure (..), Sha (..), SomeRefSpec (..), TagMessage (..), TagName (..),
            TagResult (..))

import GithubApi (GithubOperation, GithubOperationFree)
import Project (Approval (..), ApprovedFor (..), BuildStatus (..), IntegrationStatus (..),
                MergeWindow(..), ProjectState, PullRequest, PullRequestStatus (..))
import Time (TimeOperation, TimeOperationFree)
import Types (PullRequestId (..), Username (..))

import qualified Configuration as Config
import qualified Git
import qualified GithubApi
import qualified Project as Pr
import qualified Time

data ActionFree a
  = TryIntegrate
    -- This is a record type, but the names are currently only used for documentation.
    { _mergeCommitMessage   :: Text
    , _integrationCandidate :: (Branch, Sha)
    , _alwaysAddMergeCommit :: Bool
    , _cont                 :: Either IntegrationFailure Sha -> a
    }
  | TryPromote Branch Sha (PushResult -> a)
  | TryPromoteWithTag Branch Sha TagName TagMessage (PushWithTagResult -> a)
  | LeaveComment PullRequestId Text a
  | IsReviewer Username (Bool -> a)
  | GetPullRequest PullRequestId (Maybe GithubApi.PullRequest -> a)
  | GetOpenPullRequests (Maybe IntSet -> a)
  | GetLatestVersion Sha (Either TagName Integer -> a)
  | GetChangelog TagName Sha (Maybe Text -> a)
  | GetDateTime (UTCTime -> a)
  deriving (Functor)

data PRCloseCause =
      User            -- ^ The user closed the PR.
    | StopIntegration -- ^ We close and reopen the PR internally to stop its integration if it is approved.

type Action = Free ActionFree

type Operation = Free (Sum TimeOperationFree (Sum GitOperationFree GithubOperationFree))

type PushWithTagResult = (Either Text TagName, PushResult)

-- | Error returned when 'TryIntegrate' fails.
-- It contains the name of the target branch that the PR was supposed to be integrated into.

data IntegrationFailure = IntegrationFailure BaseBranch GitIntegrationFailure

doTime :: TimeOperation a -> Operation a
doTime = hoistFree InL

doGit :: GitOperation a -> Operation a
doGit = hoistFree (InR . InL)

doGithub :: GithubOperation a -> Operation a
doGithub = hoistFree (InR . InR)

tryIntegrate :: Text -> (Branch, Sha) -> Bool -> Action (Either IntegrationFailure Sha)
tryIntegrate mergeMessage candidate alwaysAddMergeCommit = liftF $ TryIntegrate mergeMessage candidate alwaysAddMergeCommit id

-- Try to fast-forward the remote target branch (usually master) to the new sha.
-- Before doing so, force-push that SHA to the pull request branch, and after
-- success, delete the pull request branch. These steps ensure that Github marks
-- the pull request as merged, rather than closed.
tryPromote :: Branch -> Sha -> Action PushResult
tryPromote prBranch newHead = liftF $ TryPromote prBranch newHead id

tryPromoteWithTag :: Branch -> Sha -> TagName -> TagMessage -> Action PushWithTagResult
tryPromoteWithTag prBranch newHead tagName tagMessage =
  liftF $ TryPromoteWithTag prBranch newHead tagName tagMessage id

-- Leave a comment on the given pull request.
leaveComment :: PullRequestId -> Text -> Action ()
leaveComment pr body = liftF $ LeaveComment pr body ()

-- Check if this user is allowed to issue merge commands.
isReviewer :: Username -> Action Bool
isReviewer username = liftF $ IsReviewer username id

getPullRequest :: PullRequestId -> Action (Maybe GithubApi.PullRequest)
getPullRequest pr = liftF $ GetPullRequest pr id

getOpenPullRequests :: Action (Maybe IntSet)
getOpenPullRequests = liftF $ GetOpenPullRequests id

getLatestVersion :: Sha -> Action (Either TagName Integer)
getLatestVersion sha = liftF $ GetLatestVersion sha id

getChangelog :: TagName -> Sha -> Action (Maybe Text)
getChangelog prevTag curHead = liftF $ GetChangelog prevTag curHead id

getDateTime :: Action UTCTime
getDateTime = liftF $ GetDateTime id

-- Interpreter that translates high-level actions into more low-level ones.
runAction :: ProjectConfiguration -> Action a -> Operation a
runAction config = foldFree $ \case
  TryIntegrate message (ref, sha) alwaysAddMergeCommit cont -> do
    doGit $ ensureCloned config
    shaOrFailed <- doGit $ Git.tryIntegrate
      message
      ref
      sha
      (Git.RemoteBranch $ Config.branch config)
      (Git.Branch $ Config.testBranch config)
      alwaysAddMergeCommit

    case shaOrFailed of
      Left failure -> pure $ cont $ Left $ IntegrationFailure (BaseBranch $ Config.branch config) failure
      Right integratedSha -> pure $ cont $ Right integratedSha

  TryPromote prBranch sha cont -> do
    doGit $ ensureCloned config
    forcePushResult <- doGit $ Git.forcePush sha prBranch
    case forcePushResult of
      PushRejected _ -> pure $ cont forcePushResult
      PushOk -> do
        pushResult <- doGit $ Git.push sha (Git.Branch $ Config.branch config)
        pure $ cont pushResult

  TryPromoteWithTag prBranch sha newTagName newTagMessage cont -> doGit $
    ensureCloned config >>
    Git.forcePush sha prBranch >>
    Git.tag sha newTagName newTagMessage >>=
    \case TagFailed _ -> cont . (Left "Please check the logs",) <$> Git.push sha (Git.Branch $ Config.branch config)
          TagOk tagName -> cont . (Right tagName,)
            <$> Git.pushAtomic [AsRefSpec tagName, AsRefSpec (sha, Git.Branch $ Config.branch config)]
            <*  Git.deleteTag tagName
            -- Deleting tag after atomic push is important to maintain one "source of truth", namely
            -- the origin

  LeaveComment pr body cont -> do
    doGithub $ GithubApi.leaveComment pr body
    pure cont

  IsReviewer username cont -> do
    hasPushAccess <- doGithub $ GithubApi.hasPushAccess username
    pure $ cont hasPushAccess

  GetPullRequest pr cont -> do
    details <- doGithub $ GithubApi.getPullRequest pr
    pure $ cont details

  GetOpenPullRequests cont -> do
    openPrIds <- doGithub GithubApi.getOpenPullRequests
    pure $ cont openPrIds

  GetLatestVersion sha cont -> doGit $
    cont . maybe (Right 0) (\t -> maybeToEither t $ parseVersion t) <$> Git.lastTag sha

  GetChangelog prevTag curHead cont -> doGit $
    cont <$> Git.shortlog (AsRefSpec prevTag) (AsRefSpec curHead)

  GetDateTime cont -> doTime $ cont <$> Time.getDateTime

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
        Git.CloneOk     -> pure ()
        Git.CloneFailed -> cloneWithRetry (triesLeft - 1)
  in do
    exists <- Git.doesGitDirectoryExist
    unless exists (cloneWithRetry 3)

data Event
  -- GitHub events
  = PullRequestOpened PullRequestId Branch BaseBranch Sha Text Username -- ^ PR, branch, base branch, sha, title, author.
  -- The commit changed event may contain false positives: it may be received
  -- even if the commit did not really change. This is because GitHub just
  -- sends a "something changed" event along with the new state.
  | PullRequestCommitChanged PullRequestId Sha -- ^ PR, new sha.
  | PullRequestClosed PullRequestId            -- ^ PR.
  | PullRequestEdited PullRequestId Text BaseBranch -- ^ PR, new title, new base branch.
  | CommentAdded PullRequestId Username Text   -- ^ PR, author and body.
  -- CI events
  | BuildStatusChanged Sha BuildStatus
  -- Internal events
  | Synchronize
  deriving (Eq, Show)

type EventQueue = TBQueue (Maybe Event)
type StateVar = TMVar ProjectState

-- Creates a new event queue with the given maximum capacity.
newEventQueue :: Natural -> IO EventQueue
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
newStateVar = newTMVarIO

-- Put a new value in the project state variable, discarding the previous one.
updateStateVar :: StateVar -> ProjectState -> IO ()
updateStateVar var state = void $ atomically $ swapTMVar var state

-- Read the most recent value from the project state variable.
readStateVar :: StateVar -> IO ProjectState
readStateVar var = atomically $ readTMVar var

-- | Closes and opens a new PR with the same id. Useful for clearing approval and build status safely.
clearPullRequest :: PullRequestId -> PullRequest -> ProjectState -> Action ProjectState
clearPullRequest prId pr state =
  let
    branch = Pr.branch pr
    title  = Pr.title pr
    author = Pr.author pr
    baseBranch = Pr.baseBranch pr
    sha    = Pr.sha pr
  in
    handlePullRequestClosed StopIntegration prId state >>=
      handlePullRequestOpened prId branch baseBranch sha title author

-- Handle a single event, but don't take any other actions. To complete handling
-- of the event, we must also call `proceed` on the state until we reach a fixed
-- point. This is handled by `handleEvent`.
handleEventInternal
  :: TriggerConfiguration
  -> ProjectConfiguration
  -> MergeWindowExemptionConfiguration
  -> Event
  -> ProjectState
  -> Action ProjectState
handleEventInternal triggerConfig projectConfig mergeWindowExemption event = case event of
  PullRequestOpened pr branch baseBranch sha title author
    -> handlePullRequestOpened pr branch baseBranch sha title author
  PullRequestCommitChanged pr sha -> handlePullRequestCommitChanged pr sha
  PullRequestClosed pr            -> handlePullRequestClosedByUser pr
  PullRequestEdited pr title baseBranch -> handlePullRequestEdited pr title baseBranch
  CommentAdded pr author body
    -> handleCommentAdded triggerConfig projectConfig mergeWindowExemption pr author body
  BuildStatusChanged sha status   -> pure . handleBuildStatusChanged sha status
  Synchronize                     -> synchronizeState

handlePullRequestOpened
  :: PullRequestId
  -> Branch
  -> BaseBranch
  -> Sha
  -> Text
  -> Username
  -> ProjectState
  -> Action ProjectState
handlePullRequestOpened pr branch baseBranch sha title author =
  return . Pr.insertPullRequest pr branch baseBranch sha title author

handlePullRequestCommitChanged :: PullRequestId -> Sha -> ProjectState -> Action ProjectState
handlePullRequestCommitChanged prId newSha state =
  -- If the commit changes, pretend that the PR was closed. This forgets about
  -- approval and build status. Then pretend a new PR was opened, with the same
  -- author as the original one, but with the new sha.
    let updateSha pr = pr { Pr.sha = newSha } in
    case Pr.lookupPullRequest prId state of
      Just pullRequest
        -- If the change notification was a false positive, ignore it.
        | Pr.sha pullRequest == newSha -> pure state
        -- If the new commit hash is one that we pushed ourselves, ignore the
        -- change too, we don't want to lose the approval status.
        | newSha `Pr.wasIntegrationAttemptFor` pullRequest -> pure state
        | otherwise -> clearPullRequest prId (updateSha pullRequest) state
      -- If the pull request was not present in the first place, do nothing.
      Nothing -> pure state

-- | Describe what caused the PR to close.
prClosingMessage :: PRCloseCause -> Text
prClosingMessage User = "Abandoning this pull request because it was closed."
prClosingMessage StopIntegration = "Stopping integration because the PR changed after approval."

-- | Handle PR close when a user actually closes a PR.
handlePullRequestClosedByUser :: PullRequestId -> ProjectState -> Action ProjectState
handlePullRequestClosedByUser = handlePullRequestClosed User

handlePullRequestClosed :: PRCloseCause -> PullRequestId -> ProjectState -> Action ProjectState
handlePullRequestClosed closingReason pr state = do
  when (pr `elem` Pr.integratedPullRequests state) $
    leaveComment pr $ prClosingMessage closingReason
  pure $ Pr.deletePullRequest pr state

handlePullRequestEdited :: PullRequestId -> Text -> BaseBranch -> ProjectState -> Action ProjectState
handlePullRequestEdited prId newTitle newBaseBranch state =
  let updatePr pr =  pr { Pr.title = newTitle, Pr.baseBranch = newBaseBranch } in
  case Pr.lookupPullRequest prId state of
    Just pullRequest
      -- If the base branch hasn't changed, just update the pull request.
      | Pr.baseBranch pullRequest == newBaseBranch -> pure $ Pr.updatePullRequest prId updatePr state
      -- If the base branch has changed, update the PR and clear the approval and build status.
      | otherwise -> clearPullRequest prId (updatePr pullRequest) state
    -- Do nothing if the pull request is not present.
    Nothing -> pure state

-- | Internal result type for parsing a merge command, which allows the
-- consumer of `parseMergeCommand` to inspect the reason why a message
-- was considered invalid.
data ParseResult a
  -- | The parser found a valid prefix and a valid command.
  = Success a
  -- | The parser found a valid prefix, but no valid command.
  | Unknown Text
  -- | The parser decided to ignore the message because it did
  -- not contain a valid prefix.
  | Ignored

-- Checks whether the parse result was valid.
isSuccess :: ParseResult a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

-- Returns the approval type contained in the given text, if the message is a
-- command that instructs us to merge the PR.
-- If the trigger prefix is "@hoffbot", a command "@hoffbot merge" would
-- indicate the `Merge` approval type.
-- Returns `Ignored` if the bot was not mentioned, `Success` if it was mentioned
-- in the same message as a valid command, and `Unknown` if it was mentioned but
-- no valid command was found.
parseMergeCommand :: TriggerConfiguration -> Text -> ParseResult (ApprovedFor, MergeWindow)
parseMergeCommand config message =
  let
    normalise :: Text -> Text
    normalise msg =
      -- Normalise commands with extra spaces between them (`@Bot  merge  and tag` | `merge and  tag`)
      let multiWhitespaceStripped = Text.unwords $ filter (not . Text.null) $ Text.words msg
      -- Standardise the casing in order to match commands with different casing (@Bot Merge)
      in Text.toCaseFold multiWhitespaceStripped

    messageNormalised = normalise message
    prefixNormalised = normalise $ Config.commentPrefix config
    mentioned = prefixNormalised `Text.isPrefixOf` messageNormalised
    -- Determines if any individual mention matches the given command message
    matchWith :: Text -> Bool
    matchWith msg = any (Text.isPrefixOf msg) mentions
      where mentions = Text.splitOn prefixNormalised messageNormalised
    -- Check if the prefix followed by ` merge [and {deploy,tag}] [on friday]` occurs within the message.
    -- We opt to include the space here, instead of making it part of the
    -- prefix, because having the trailing space in config is something that is
    -- easy to get wrong.
    -- Note that because "merge" is an infix of "merge and xxx" we need to
    -- check for the "merge and xxx" commands first: if this order were
    -- reversed all "merge and xxx" commands would be detected as a Merge
    -- command.
    cases =
      [ (" merge and deploy on friday", (MergeAndDeploy, OnFriday)),
        (" merge and deploy", (MergeAndDeploy, NotFriday)),
        (" merge and tag on friday", (MergeAndTag, OnFriday)),
        (" merge and tag", (MergeAndTag, NotFriday)),
        (" merge on friday", (Merge, OnFriday)),
        (" merge", (Merge, NotFriday))
      ]
   in case listToMaybe [ cmd | (msg, cmd) <- cases, matchWith msg ] of
     Just command -> Success command
     Nothing | mentioned -> Unknown message
             | otherwise -> Ignored


-- Mark the pull request as approved, and leave a comment to acknowledge that.
approvePullRequest :: PullRequestId -> Approval -> ProjectState -> Action ProjectState
approvePullRequest pr approval = pure . Pr.updatePullRequest pr
    (\pullRequest -> pullRequest
      { Pr.approval = Just approval
      , Pr.needsFeedback = True
      })

handleCommentAdded
  :: TriggerConfiguration
  -> ProjectConfiguration
  -> MergeWindowExemptionConfiguration
  -> PullRequestId
  -> Username
  -> Text
  -> ProjectState
  -> Action ProjectState
handleCommentAdded triggerConfig projectConfig mergeWindowExemption prId author body state =
  let maybePR = Pr.lookupPullRequest prId state in
  case maybePR of
    -- Check if the comment is a merge command, and if it is, check if the
    -- author is allowed to approve. Comments by users with push access happen
    -- frequently, but most comments are not merge commands, and checking that
    -- a user has push access requires an API call.
    Just pr -> do
      let commandType = parseMergeCommand triggerConfig body
          exempted :: Username -> Bool
          exempted (Username user) =
            let (Config.MergeWindowExemptionConfiguration users) = mergeWindowExemption
            in elem user users
      -- Check whether the author is allowed to do merge commands, but only if
      -- a valid command was parsed.
      isAllowed <- if isSuccess commandType
        then isReviewer author
        else pure False
      -- For now Friday at UTC+0 is good enough.
      -- See https://github.com/channable/hoff/pull/95 for caveats and improvement ideas.
      day <- dayOfWeek . utctDay <$> getDateTime
      case commandType of
        -- The bot was not mentioned in the comment, ignore
        Ignored -> pure state
        -- The bot was mentioned but encountered an invalid command, report error and
        -- take no further action
        Unknown command -> do
          let prefix  = Text.toCaseFold $ Config.commentPrefix triggerConfig
              cmdstr  = fmap Text.strip $ Text.stripPrefix prefix command
              comment = case cmdstr of
                Just str -> "`" <> str <> "` was not recognized as a valid command."
                Nothing  -> "That was not a valid command."
          () <- leaveComment prId comment
          pure state
        -- Cases where the parse was successful
        Success command
          -- Author is a reviewer
          | isAllowed -> case command of
            -- To guard against accidental merges we make use of a merge window.
            -- Merging inside this window is discouraged but can be overruled with a special command or by adding the
            -- user to the merge window exemption list.
            (approval, _) | exempted author -> handleMergeRequested projectConfig prId author state pr approval
            (approval, OnFriday) | day == Friday -> handleMergeRequested projectConfig prId author state pr approval
            (approval, NotFriday)| day /= Friday -> handleMergeRequested projectConfig prId author state pr approval
            (other, NotFriday) -> do
              () <- leaveComment prId ("Your merge request has been denied, because \
                                        \merging on Fridays is not recommended. \
                                        \To override this behaviour use the command `"
                                        <> Pr.displayApproval other <> " on Friday`.")
              pure state
            (other, OnFriday) -> do
              () <- leaveComment prId ("Your merge request has been denied because \
                                        \it is not Friday. Run " <>
                                        Pr.displayApproval other <> " instead")
              pure state
          -- Author is not a reviewer, so we ignore
          | otherwise -> pure state
    -- If the pull request is not in the state, ignore the comment.
    Nothing -> pure state

handleMergeRequested
  :: ProjectConfiguration
  -> PullRequestId
  -> Username
  -> ProjectState
  -> PullRequest
  -> ApprovedFor
  -> Action ProjectState
handleMergeRequested projectConfig prId author state pr approvalType = do
  let (order, state') = Pr.newApprovalOrder state
  state'' <- approvePullRequest prId (Approval author approvalType order) state'
  -- Check whether the integration branch is valid, if not, mark the integration as invalid.
  if Pr.baseBranch pr /= BaseBranch (Config.branch projectConfig)
    then pure $ Pr.setIntegrationStatus prId IncorrectBaseBranch state''
    else pure state''

handleBuildStatusChanged :: Sha -> BuildStatus -> ProjectState -> ProjectState
handleBuildStatusChanged buildSha newStatus state =
  -- If there is an integration candidate, and its integration sha matches that
  -- of the build, then update the build status for that pull request. Otherwise
  -- do nothing.
  let
    setBuildStatus pullRequest = case Pr.integrationStatus pullRequest of
      Integrated candidateSha _oldStatus | candidateSha == buildSha ->
        pullRequest { Pr.integrationStatus = Integrated buildSha newStatus }
      _ -> pullRequest
  in
    case Pr.integrationCandidate state of
      Just candidateId -> Pr.updatePullRequest candidateId setBuildStatus state
      Nothing          -> state

-- Query the GitHub API to resolve inconsistencies between our state and GitHub.
synchronizeState :: ProjectState -> Action ProjectState
synchronizeState stateInitial =
  getOpenPullRequests >>= \case
    -- If we fail to obtain the currently open pull requests from GitHub, then
    -- the synchronize event is a no-op, we keep the current state.
    Nothing -> pure stateInitial
    Just externalOpenPrIds -> do
      let
        internalOpenPrIds = IntMap.keysSet $ Pr.pullRequests stateInitial
        -- We need to convert to a list because IntSet has no Foldable instance.
        toList = fmap PullRequestId . IntSet.toList
        prsToClose = toList $ IntSet.difference internalOpenPrIds externalOpenPrIds
        prsToOpen  = toList $ IntSet.difference externalOpenPrIds internalOpenPrIds

        insertMissingPr state pr = getPullRequest pr >>= \case
          -- On error, ignore this pull request.
          Nothing -> pure state
          Just details -> pure $ Pr.insertPullRequest
            pr
            (GithubApi.branch details)
            (GithubApi.baseBranch details)
            (GithubApi.sha details)
            (GithubApi.title details)
            (GithubApi.author details)
            state

      -- Close all pull requests that are still open internally (in our state),
      -- but which are not open externally (on GitHub).
      stateClosed <- foldM (flip handlePullRequestClosedByUser) stateInitial prsToClose
      -- Then get the details for all pull requests that are open on GitHub, but
      -- which are not yet in our state, and add them.
      foldM insertMissingPr stateClosed prsToOpen

-- Determines if there is anything to do, and if there is, generates the right
-- actions and updates the state accordingly. For example, if the current
-- integration candidate has been integrated (and is no longer a candidate), we
-- should find a new candidate. Or after the pull request for which a build is
-- in progress is closed, we should find a new candidate.
proceed :: ProjectState -> Action ProjectState
proceed state = do
  state' <- provideFeedback state
  case Pr.getIntegrationCandidate state' of
    Just candidate -> proceedCandidate candidate state'
    -- No current integration candidate, find the next one.
    Nothing -> case Pr.candidatePullRequests state' of
      -- No pull requests eligible, do nothing.
      []     -> return state'
      -- Found a new candidate, try to integrate it.
      pr : _ -> tryIntegratePullRequest pr state'

-- TODO: Get rid of the tuple; just pass the ID and do the lookup with fromJust.
proceedCandidate :: (PullRequestId, PullRequest) -> ProjectState -> Action ProjectState
proceedCandidate (pullRequestId, pullRequest) state =
  case Pr.integrationStatus pullRequest of
    NotIntegrated ->
      tryIntegratePullRequest pullRequestId state

    IncorrectBaseBranch ->
      -- It shouldn't come to this point; a PR with an incorrect base branch is
      -- never considered as a candidate.
      pure $ Pr.setIntegrationCandidate Nothing state

    Conflicted _branch _ ->
      -- If it conflicted, it should no longer be the integration candidate.
      pure $ Pr.setIntegrationCandidate Nothing state

    Integrated sha buildStatus -> case buildStatus of
      BuildPending   -> pure state
      BuildSucceeded -> pushCandidate (pullRequestId, pullRequest) sha state
      BuildFailed _  -> do
        -- If the build failed, this is no longer a candidate.
        pure $ Pr.setIntegrationCandidate Nothing $
          Pr.setNeedsFeedback pullRequestId True state

    Promoted -> pure state

-- Given a pull request id, returns the name of the GitHub ref for that pull
-- request, so it can be fetched.
getPullRequestRef :: PullRequestId -> Branch
getPullRequestRef (PullRequestId n) = Branch $ format "refs/pull/{}/head" [n]

-- Integrates proposed changes from the pull request into the target branch.
-- The pull request must exist in the project.
tryIntegratePullRequest :: PullRequestId -> ProjectState -> Action ProjectState
tryIntegratePullRequest pr state =
  let
    PullRequestId prNumber = pr
    pullRequest  = fromJust $ Pr.lookupPullRequest pr state
    title = Pr.title pullRequest
    Approval (Username approvedBy) approvalType _prOrder = fromJust $ Pr.approval pullRequest
    candidateSha = Pr.sha pullRequest
    candidateRef = getPullRequestRef pr
    candidate = (candidateRef, candidateSha)
    mergeMessageLines =
      [ format "Merge #{}: {}" (prNumber, title)
      , ""
      , format "Approved-by: {}" [approvedBy]
      , format "Auto-deploy: {}" [if approvalType == MergeAndDeploy then "true" else "false" :: Text]
      ]
    mergeMessage = Text.unlines mergeMessageLines
  in do
    result <- tryIntegrate mergeMessage candidate $ Pr.alwaysAddMergeCommit approvalType
    case result of
      Left (IntegrationFailure targetBranch reason) ->
        -- If integrating failed, perform no further actions but do set the
        -- state to conflicted.
        pure $ Pr.setIntegrationStatus pr (Conflicted targetBranch reason) $
          Pr.setNeedsFeedback pr True state

      Right (Sha sha) -> do
        -- If it succeeded, update the integration candidate, and set the build
        -- to pending, as pushing should have triggered a build.
        pure
          $ Pr.setIntegrationStatus pr (Integrated (Sha sha) BuildPending)
          $ Pr.setIntegrationCandidate (Just pr)
          $ Pr.setNeedsFeedback pr True
          $ state

-- Pushes the integrated commits of the given candidate pull request to the
-- target branch. If the push fails, restarts the integration cycle for the
-- candidate.
-- TODO: Get rid of the tuple; just pass the ID and do the lookup with fromJust.
pushCandidate :: (PullRequestId, PullRequest) -> Sha -> ProjectState -> Action ProjectState
pushCandidate (pullRequestId, pullRequest) newHead state =
  -- Look up the sha that will be pushed to the target branch. Also assert that
  -- the pull request has really been approved. If it was
  -- not, there is a bug in the program.
  let prBranch  = Pr.branch pullRequest
      approval = fromJust $ Pr.approval pullRequest
      Username approvedBy = approver approval
      commentToUser = leaveComment pullRequestId . (<>) ("@" <> approvedBy <> " ")
  in assert (isJust $ Pr.approval pullRequest) $ do
    let approvalKind = Pr.approvedFor approval
    pushResult <- if Pr.needsTag $ approvalKind
      then do
        versionOrBadTag <- getLatestVersion newHead
        case versionOrBadTag of
          Left (TagName bad) -> do
            commentToUser ("Sorry, I could not tag your PR. The previous tag `" <> bad <> "` seems invalid")
            tryPromote prBranch newHead
          Right v -> do
            let previousTag = versionToTag v
            mChangelog <- getChangelog previousTag newHead
            let
              tagName = versionToTag $ v + 1
              changelog = maybe "Failed to get the changelog" id mChangelog
              tagMessage = messageForTag tagName approvalKind changelog
            (tagResult, pushResult) <- tryPromoteWithTag prBranch newHead tagName tagMessage
            when (pushResult == PushOk) $ commentToUser $
              case tagResult of
                Left err -> "Sorry, I could not tag your PR. " <> err
                Right (TagName t) -> "I tagged your PR with `" <> t <> "`. " <>
                  if Pr.needsDeploy approvalKind
                    then "It is scheduled for autodeploy!"
                    else "Don't forget to deploy it!"
            pure pushResult
      else
        tryPromote prBranch newHead
    case pushResult of
      -- If the push worked, then this was the final stage of the pull request.
      -- GitHub will mark the pull request as closed, and when we receive that
      -- event, we delete the pull request from the state. Until then, reset
      -- the integration candidate, so we proceed with the next pull request.
      PushOk -> pure $ Pr.setIntegrationStatus pullRequestId Promoted state
      -- If something was pushed to the target branch while the candidate was
      -- being tested, try to integrate again and hope that next time the push
      -- succeeds.
      PushRejected _why -> tryIntegratePullRequest pullRequestId state

-- Keep doing a proceed step until the state doesn't change any more. For this
-- to work properly, it is essential that "proceed" does not have any side
-- effects if it does not change the state.
proceedUntilFixedPoint :: ProjectState -> Action ProjectState
proceedUntilFixedPoint state = do
  newState <- proceed state
  if newState == state
    then return state
    else proceedUntilFixedPoint newState

-- Describe the status of the pull request.
describeStatus :: PullRequestId -> PullRequest -> ProjectState -> Text
describeStatus prId pr state = case Pr.classifyPullRequest pr of
  PrStatusAwaitingApproval -> "Pull request awaiting approval."
  PrStatusApproved ->
    let
      Approval (Username approvedBy) approvalType _position = fromJust $ Pr.approval pr
      approvalCommand = Pr.displayApproval approvalType
    in case Pr.getQueuePosition prId state of
      0 -> format "Pull request approved for {} by @{}, rebasing now." [approvalCommand, approvedBy]
      1 -> format "Pull request approved for {} by @{}, waiting for rebase behind one pull request." [approvalCommand, approvedBy]
      n -> format "Pull request approved for {} by @{}, waiting for rebase behind {} pull requests." (approvalCommand, approvedBy, n)
  PrStatusBuildPending ->
    let Sha sha = fromJust $ getIntegrationSha pr
    in Text.concat ["Rebased as ", sha, ", waiting for CI …"]
  PrStatusIntegrated -> "The build succeeded."
  PrStatusIncorrectBaseBranch -> "Merge rejected: the target branch must be the integration branch."
  PrStatusWrongFixups -> "Pull request cannot be integrated as it contains fixup commits that do not belong to any other commits."
  PrStatusEmptyRebase -> "Empty rebase. \
                         \ Have the changes already been merged into the target branch? \
                         \ Aborting."
  PrStatusFailedConflict ->
    let
      BaseBranch targetBranchName = Pr.baseBranch pr
      Branch prBranchName = Pr.branch pr
    in Text.concat
      [ "Failed to rebase, please rebase manually using\n\n"
      , "    git rebase --interactive --autosquash origin/"
      , targetBranchName
      , " "
      , prBranchName
      ]
  PrStatusFailedBuild url -> case url of
                              Just url' -> format "The build failed: {}\nIf this is the result of a flaky test, close and reopen the PR, then tag me again.\nOtherwise, push a new commit and tag me again." [url']
                              -- This should probably never happen
                              Nothing   -> "The build failed, but GitHub did not provide an URL to the build failure."
  where
    getIntegrationSha :: PullRequest -> Maybe Sha
    getIntegrationSha pullRequest =
      case Pr.integrationStatus pullRequest of
        Integrated sha _ -> Just sha
        _                -> Nothing

-- Leave a comment with the feedback from 'describeStatus' and set the
-- 'needsFeedback' flag to 'False'.
leaveFeedback :: (PullRequestId, PullRequest) -> ProjectState -> Action ProjectState
leaveFeedback (prId, pr) state = do
  () <- leaveComment prId $ describeStatus prId pr state
  pure $ Pr.setNeedsFeedback prId False state

-- Run 'leaveFeedback' on all pull requests that need feedback.
provideFeedback :: ProjectState -> Action ProjectState
provideFeedback state
  = foldM (flip leaveFeedback) state
  $ filter (Pr.needsFeedback . snd)
  $ fmap (first PullRequestId)
  $ IntMap.toList $ Pr.pullRequests state

handleEvent
  :: TriggerConfiguration
  -> ProjectConfiguration
  -> MergeWindowExemptionConfiguration
  -> Event
  -> ProjectState
  -> Action ProjectState
handleEvent triggerConfig projectConfig mergeWindowExemption event state =
  handleEventInternal triggerConfig projectConfig mergeWindowExemption event state >>= proceedUntilFixedPoint


-- TODO this is very Channable specific, perhaps it should be more generic
parseVersion :: TagName -> Maybe Integer
parseVersion (TagName t) = Text.uncons t >>= \case
    ('v', num) -> parseNum num
    _          -> Nothing
  where
    parseNum num = case Text.decimal num of
      Right (v, remaining) | Text.null (Text.strip remaining) -> Just v
      _                                                       -> Nothing

versionToTag :: Integer -> TagName
versionToTag v = TagName $ toStrict $ B.toLazyText $ B.singleton 'v' <> B.decimal v

messageForTag :: TagName -> ApprovedFor -> Text -> TagMessage
messageForTag (TagName tagName) tagOrDeploy changelog =
  TagMessage $ tagName <> mark <> "\n\n" <> changelog
  where mark = if Pr.needsDeploy tagOrDeploy then " (autodeploy)" else ""
