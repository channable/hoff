-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Logic
(
  -- Action,
  Action (..),
  Event (..),
  EventQueue,
  IntegrationFailure (..),
  StateVar,
  RetrieveEnvironment (..),
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
  runRetrieveEnvironment,
  tryIntegratePullRequest,
  updateStateVar,
)
where

import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Control.Concurrent.STM.TMVar (TMVar, newTMVarIO, readTMVar, swapTMVar)
import Control.Exception (assert)
import Control.Monad (foldM, unless, void, when, (>=>))
import Control.Monad.STM (atomically)
import Data.Bifunctor (first)
import Data.Either.Extra (maybeToEither)
import Data.IntSet (IntSet)
import Data.List (intercalate, intersperse)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Void (Void)
import GHC.Natural (Natural)
import Text.Megaparsec (ParseErrorBundle, Parsec, (<|>))

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Read as Text
import Data.Time (UTCTime, DayOfWeek (Friday), dayOfWeek, utctDay)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import Configuration (ProjectConfiguration (owner, repository, deployEnvironments), TriggerConfiguration, MergeWindowExemptionConfiguration, FeatureFreezeWindow, PromotionTimeout)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Format (format)
import Git (Branch (..), BaseBranch (..), GitOperation, PushResult (..),
            GitIntegrationFailure (..), Sha (..), SomeRefSpec (..), TagMessage (..), TagName (..),
            TagResult (..), Context)

import GithubApi (GithubOperation)
import Metrics.Metrics (MetricsOperation, increaseMergedPRTotal, updateTrainSizeGauge)
import Project (Approval (..), ApprovedFor (..), MergeCommand (..), BuildStatus (..), Check (..), DeployEnvironment (..), IntegrationStatus (..),
                MergeWindow(..), ProjectState, PullRequest, PullRequestStatus (..),
                summarize, supersedes)
import Time (TimeOperation)
import Types (Body (..), PullRequestId (..), Username (..))

import qualified Configuration as Config
import qualified Git
import qualified GithubApi
import qualified Project as Pr
import qualified Time

-- | Represents the high level manipulations of a pull request
data Action :: Effect where
    -- This is a record type, but the names are currently only used for documentation.
  TryIntegrate ::
    { _mergeCommitMessage   :: Text
    , _integrationCandidate :: (PullRequestId, Branch, Sha)
    , _train                :: [PullRequestId]
    , _alwaysAddMergeCommit :: Bool
    } -> Action m (Either IntegrationFailure Sha)
  TryForcePush :: Branch -> Sha -> Action m PushResult
  TryPromote :: Sha -> Action m PushResult
  TryPromoteWithTag :: Sha -> TagName -> TagMessage -> Action m PushWithTagResult
  CleanupTestBranch :: PullRequestId -> Action m ()
  LeaveComment :: PullRequestId -> Text -> Action m ()
  IsReviewer :: Username -> Action m Bool
  GetPullRequest :: PullRequestId -> Action m (Maybe GithubApi.PullRequest)
  GetOpenPullRequests :: Action m (Maybe IntSet)
  GetLatestVersion :: Sha -> Action m (Either TagName Integer)
  GetChangelog :: TagName -> Sha -> Action m (Maybe Text)
  IncreaseMergeMetric :: Action m ()
  UpdateTrainSizeMetric :: Int -> Action m ()

type instance DispatchOf Action = 'Dynamic

-- | Get values from the context we're in
data RetrieveEnvironment :: Effect where
  GetProjectConfig :: RetrieveEnvironment m ProjectConfiguration
  GetDateTime :: RetrieveEnvironment m UTCTime
  GetBaseBranch :: RetrieveEnvironment m BaseBranch

type instance DispatchOf RetrieveEnvironment = 'Dynamic

data PRCloseCause =
      User            -- ^ The user closed the PR.
    | StopIntegration -- ^ We close and reopen the PR internally to stop its integration if it is approved.
  deriving Show

type PushWithTagResult = (Either Text TagName, PushResult)

-- | Error returned when 'TryIntegrate' fails.
-- It contains the name of the target branch that the PR was supposed to be integrated into.
data IntegrationFailure = IntegrationFailure BaseBranch GitIntegrationFailure

tryIntegrate
  :: Action :> es
  => Text
  -> (PullRequestId, Branch, Sha)
  -> [PullRequestId]
  -> Bool
  -> Eff es (Either IntegrationFailure Sha)
tryIntegrate mergeMessage candidate train alwaysAddMergeCommit =
  send $ TryIntegrate mergeMessage candidate train alwaysAddMergeCommit

-- | Try to fast-forward the remote target branch (usually master) to the new sha.
-- Before doing so, force-push that SHA to the pull request branch, and after
-- success, delete the pull request branch. These steps ensure that Github marks
-- the pull request as merged, rather than closed.

-- | Try to force push a new commit to a specific branch.
tryForcePush :: Action :> es => Branch -> Sha -> Eff es PushResult
tryForcePush prBranch newHead = send $ TryForcePush prBranch newHead

-- | Try to push a new commit to the master branch.
tryPromote :: Action :> es => Sha -> Eff es PushResult
tryPromote newHead = send $ TryPromote newHead

-- | Try to push a new commit to the master branch and push a new tag.
tryPromoteWithTag :: Action :> es => Sha -> TagName -> TagMessage -> Eff es PushWithTagResult
tryPromoteWithTag newHead tagName tagMessage =
  send $ TryPromoteWithTag newHead tagName tagMessage

-- | Clean up a branch that has been merged.
cleanupTestBranch :: Action :> es => PullRequestId -> Eff es ()
cleanupTestBranch pullRequestId = send $ CleanupTestBranch pullRequestId

-- | Leave a comment on the given pull request.
leaveComment :: Action :> es => PullRequestId -> Text -> Eff es ()
leaveComment pr body = send $ LeaveComment pr body

-- | Check if this user is allowed to issue merge commands.
isReviewer :: Action :> es => Username -> Eff es Bool
isReviewer username = send $ IsReviewer username

getPullRequest :: Action :> es => PullRequestId -> Eff es (Maybe GithubApi.PullRequest)
getPullRequest pr = send $ GetPullRequest pr

getOpenPullRequests :: Action :> es => Eff es (Maybe IntSet)
getOpenPullRequests = send GetOpenPullRequests

getLatestVersion :: Action :> es => Sha -> Eff es (Either TagName Integer)
getLatestVersion sha = send $ GetLatestVersion sha

getChangelog :: Action :> es => TagName -> Sha -> Eff es (Maybe Text)
getChangelog prevTag curHead = send $ GetChangelog prevTag curHead

getDateTime :: RetrieveEnvironment :> es => Eff es UTCTime
getDateTime = send GetDateTime

getBaseBranch :: RetrieveEnvironment :> es => Eff es BaseBranch
getBaseBranch = send GetBaseBranch

getProjectConfig :: RetrieveEnvironment :> es => Eff es ProjectConfiguration
getProjectConfig = send GetProjectConfig

registerMergedPR :: Action :> es => Eff es ()
registerMergedPR = send IncreaseMergeMetric

triggerTrainSizeUpdate :: Action :> es => ProjectState -> Eff es ()
triggerTrainSizeUpdate projectState = do
  let n = IntMap.size $ IntMap.filter Pr.isInProgress (Pr.pullRequests projectState)
  send $ UpdateTrainSizeMetric n

-- | Interpreter that translates high-level actions into more low-level ones.
runAction
  :: (MetricsOperation :> es, GitOperation :> es, GithubOperation :> es)
  => ProjectConfiguration
  -> Eff (Action : es) a
  -> Eff es a
runAction config =
  interpret $ \_ -> \case
    TryIntegrate message (pr, ref, sha) train alwaysAddMergeCommit -> do
      ensureCloned config

      let targetBranch = fromMaybe (Git.Branch $ Config.branch config) (trainBranch train)

      shaOrFailed <- Git.tryIntegrate
        message
        ref
        sha
        (Git.toRemoteBranch targetBranch)
        (testBranch config pr)
        alwaysAddMergeCommit

      case shaOrFailed of
        Left failure -> pure $ Left $ IntegrationFailure (Git.toBaseBranch targetBranch) failure
        Right integratedSha -> pure $ Right integratedSha
    TryForcePush prBranch sha -> do
      ensureCloned config
      Git.forcePush sha prBranch

    TryPromote sha -> do
      ensureCloned config
      Git.push sha (Git.Branch $ Config.branch config)

    TryPromoteWithTag sha newTagName newTagMessage -> do
      ensureCloned config
      tagResult <- Git.tag sha newTagName newTagMessage
      case tagResult of
        TagFailed _ -> do
          pushResult <- Git.push sha (Git.Branch $ Config.branch config)
          pure (Left "Please check the logs", pushResult)
        TagOk tagName -> do
          atomicPushResult <- Git.pushAtomic [AsRefSpec tagName, AsRefSpec (sha, Git.Branch $ Config.branch config)]
          Git.deleteTag tagName
          pure (Right tagName, atomicPushResult)
          -- Deleting tag after atomic push is important to maintain one "source of truth", namely
          -- the origin

    CleanupTestBranch pr -> do
      let branch = testBranch config pr
      Git.deleteBranch branch
      void $ Git.deleteRemoteBranch branch

    LeaveComment pr body -> do
      GithubApi.leaveComment pr body

    IsReviewer username -> do
      GithubApi.hasPushAccess username

    GetPullRequest pr -> do
      GithubApi.getPullRequest pr

    GetOpenPullRequests -> do
      GithubApi.getOpenPullRequests

    GetLatestVersion sha -> do
      Git.fetchBranchWithTags $ Branch (Config.branch config)
      maybe (Right 0) (\t -> maybeToEither t $ parseVersion t) <$> Git.lastTag sha

    GetChangelog prevTag curHead ->
      Git.shortlog (AsRefSpec prevTag) (AsRefSpec curHead)

    IncreaseMergeMetric -> increaseMergedPRTotal

    UpdateTrainSizeMetric n -> updateTrainSizeGauge n

  where
    trainBranch :: [PullRequestId] -> Maybe Git.Branch
    trainBranch [] = Nothing
    trainBranch train = Just $ last [testBranch config pr | pr <- train]

runRetrieveEnvironment :: TimeOperation :> es => ProjectConfiguration -> Eff (RetrieveEnvironment : es) a -> Eff es a
runRetrieveEnvironment config = interpret $ \_ -> \case
    GetProjectConfig -> pure config

    GetDateTime -> Time.getDateTime

    GetBaseBranch -> pure $ BaseBranch (Config.branch config)

ensureCloned :: GitOperation :> es => ProjectConfiguration -> Eff es ()
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
  = PullRequestOpened PullRequestId Branch BaseBranch Sha Text Username (Maybe Body) -- ^ PR, branch, base branch, sha, title, author, body.
  -- The commit changed event may contain false positives: it may be received
  -- even if the commit did not really change. This is because GitHub just
  -- sends a "something changed" event along with the new state.
  | PullRequestCommitChanged PullRequestId Sha -- ^ PR, new sha.
  | PullRequestClosed PullRequestId            -- ^ PR.
  | PullRequestEdited PullRequestId Text BaseBranch -- ^ PR, new title, new base branch.
  | CommentAdded PullRequestId Username Text   -- ^ PR, author and body.
  | PushPerformed BaseBranch Sha               -- ^ branch, sha
  -- CI events
  | BuildStatusChanged Sha Context BuildStatus
  -- ^ sha, possible mandatory check that was submitted with the status update, new build status
  -- Internal events
  | Synchronize
  | ClockTick UTCTime
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
clearPullRequest :: Action :> es => PullRequestId -> PullRequest -> ProjectState -> Eff es ProjectState
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
  :: (Action :> es, RetrieveEnvironment :> es)
  => TriggerConfiguration
  -> MergeWindowExemptionConfiguration
  -> Maybe FeatureFreezeWindow
  -> PromotionTimeout
  -> Event
  -> ProjectState
  -> Eff es ProjectState
handleEventInternal triggerConfig mergeWindowExemption featureFreezeWindow promotionTimeout event = case event of
  PullRequestOpened pr branch baseBranch sha title author body
    -> handlePullRequestOpenedByUser triggerConfig mergeWindowExemption featureFreezeWindow pr branch baseBranch sha title author body
  PullRequestCommitChanged pr sha -> handlePullRequestCommitChanged pr sha
  PullRequestClosed pr            -> handlePullRequestClosedByUser pr
  PullRequestEdited pr title baseBranch -> handlePullRequestEdited pr title baseBranch
  CommentAdded pr author body
    -> handleCommentAdded triggerConfig mergeWindowExemption featureFreezeWindow pr author body
  BuildStatusChanged sha context status   -> handleBuildStatusChanged sha context status
  PushPerformed branch sha        -> handleTargetChanged branch sha
  Synchronize                     -> synchronizeState
  ClockTick currTime              -> handleClockTickUpdate promotionTimeout currTime

handlePullRequestOpenedByUser
  :: forall es. (Action :> es, RetrieveEnvironment :> es)
  => TriggerConfiguration
  -> MergeWindowExemptionConfiguration
  -> Maybe FeatureFreezeWindow
  -> PullRequestId
  -> Branch
  -> BaseBranch
  -> Sha
  -> Text
  -> Username
  -> Maybe Body
  -> ProjectState
  -> Eff es ProjectState
handlePullRequestOpenedByUser triggerConfig mergeWindowExemption featureFreezeWindow pr branch baseBranch sha title author body state = do
  state' <- handlePullRequestOpened pr branch baseBranch sha title author state
  case body of
    Just (Body b) -> handleCommentAdded triggerConfig mergeWindowExemption featureFreezeWindow pr author b state'
    Nothing -> pure state'

handlePullRequestOpened
  :: PullRequestId
  -> Branch
  -> BaseBranch
  -> Sha
  -> Text
  -> Username
  -> ProjectState
  -> Eff es ProjectState
handlePullRequestOpened pr branch baseBranch sha title author =
  return . Pr.insertPullRequest pr branch baseBranch sha title author

handlePullRequestCommitChanged
  :: (Action :> es, RetrieveEnvironment :> es)
  => PullRequestId
  -> Sha
  -> ProjectState
  -> Eff es ProjectState
handlePullRequestCommitChanged prId newSha state =
  -- If the commit changes, pretend that the PR was closed. This forgets about
  -- approval and build status. Then pretend a new PR was opened, with the same
  -- author as the original one, but with the new sha.
    let updateSha pr = pr { Pr.sha = newSha } in
    case Pr.lookupPullRequest prId state of
      Just pullRequest
        -- If this is the result of our force push, we promote the PR.
        | Pr.promotionSha pullRequest == Just newSha -> tryPromotePullRequest pullRequest prId state
        -- If the change notification was a false positive, ignore it.
        | Pr.sha pullRequest == newSha -> pure state
        -- If the new commit hash is one that we pushed ourselves, ignore the
        -- change too, we don't want to lose the approval status.
        | newSha `Pr.wasIntegrationAttemptFor` pullRequest -> pure state
        | otherwise -> clearPullRequest prId (updateSha pullRequest) state
      -- If the pull request was not present in the first place, do nothing.
      Nothing -> pure state

-- | Try to push the final result of a pull request to the target branch.
tryPromotePullRequest :: (Action :> es, RetrieveEnvironment :> es) => PullRequest -> PullRequestId -> ProjectState -> Eff es ProjectState
tryPromotePullRequest pullRequest prId state = do
  pushResult <- case Pr.integrationStatus pullRequest of
    -- If we only need to promote, we can just try pushing.
    Pr.Promote _ sha -> tryPromote sha
    -- If we also want to tag the PR we additionally need to handle the result of promoting and tagging.
    -- Specifically, we need to leave a comment about the result of the tag.
    Pr.PromoteAndTag _ sha@(Sha shaText) tagName tagMessage -> do
      (tagResult, pushResult) <- tryPromoteWithTag sha tagName tagMessage
      let approval = fromJust $ Pr.approval pullRequest
          Username approvedBy = approver approval
          approvalKind = Pr.approvedFor approval
      config <- getProjectConfig
      when (pushResult == PushOk) $ leaveComment prId . (<>) ("@" <> approvedBy <> " ") $
        case tagResult of
          Left err -> "Sorry, I could not tag your PR. " <> err
          Right (TagName t) -> do
            let link = format "[{}](https://github.com/{}/{}/releases/tag/{})" (t, owner config, repository config, t)
            "I tagged your PR with " <> link <> ". " <>
              if Pr.needsDeploy approvalKind
              then "It is scheduled for autodeploy!"
              else Text.concat ["Please wait for the build of ", shaText, " to pass and don't forget to deploy it!"]
      pure pushResult
    _ -> error ""
  case pushResult of
    -- If the push worked, then this was the final stage of the pull request.
    -- GitHub will mark the pull request as closed, and when we receive that
    -- event, we delete the pull request from the state. Until then, reset
    -- the integration candidate, so we proceed with the next pull request.
    PushOk -> do
      cleanupTestBranch prId
      registerMergedPR
      pure $ Pr.updatePullRequests (unspeculateConflictsAfter pullRequest)
          $ Pr.updatePullRequests (unspeculateFailuresAfter pullRequest)
          $ Pr.setIntegrationStatus prId Promoted state
    -- If something was pushed to the target branch while the candidate was
    -- being tested, try to integrate again and hope that next time the push
    -- succeeds.  We also cancel integrations in the merge train.
    -- These should be automatically restarted when we 'proceed'.
    PushRejected _why -> tryIntegratePullRequest prId
                      $ unintegrateAfter prId state

-- | Describe what caused the PR to close.
prClosingMessage :: PRCloseCause -> Text
prClosingMessage User = "Abandoning this pull request because it was closed."
prClosingMessage StopIntegration = "Stopping integration because the PR changed after approval."

-- | Handle PR close when a user actually closes a PR.
handlePullRequestClosedByUser
  :: Action :> es
  => PullRequestId
  -> ProjectState
  -> Eff es ProjectState
handlePullRequestClosedByUser = handlePullRequestClosed User

handlePullRequestClosed
  :: Action :> es
  => PRCloseCause
  -> PullRequestId
  -> ProjectState
  -> Eff es ProjectState
handlePullRequestClosed closingReason pid state =
  case Pr.lookupPullRequest pid state of
  Nothing -> pure state
  Just pr -> do
    let status = Pr.integrationStatus pr
    when (Pr.isUnfailedIntegrated status) $
      leaveComment pid $ prClosingMessage closingReason
    when (Pr.isIntegrated status) $
      cleanupTestBranch pid
    pure $ Pr.deletePullRequest pid $
      -- if this PR is part of the train
      -- (i.e. is integrated and has not failed yet),
      -- we need to unintegrate PRs that are integrated on top of it.
      if Pr.isUnfailedIntegrated status
      then unintegrateAfter pid state
      else state

handlePullRequestEdited
  :: Action :> es
  => PullRequestId
  -> Text
  -> BaseBranch
  -> ProjectState
  -> Eff es ProjectState
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

-- | When we receive a push event, find all PRs
-- that are in progress and have the same base
-- branch. Reset their integration status to @Outdated@.
--
-- Avoid re-integrating due to pushing our own
-- SHAs of successfully integrated PRs.
handleTargetChanged
  :: forall es. (Action :> es, RetrieveEnvironment :> es)
  => BaseBranch
  -> Sha
  -> ProjectState
  -> Eff es ProjectState
handleTargetChanged (BaseBranch baseBranch) sha state
  | Just branch <- Text.stripPrefix "refs/heads/" baseBranch
  , sha `notElem` concatMap Pr.integrationShas (Pr.pullRequests state) =
  let
    update pr
      | Pr.isInProgress pr
      , Pr.baseBranch pr == BaseBranch branch
      = pr
        { Pr.integrationStatus = Outdated
        , Pr.needsFeedback = True
        }
    update pr = pr
  in pure $ Pr.updatePullRequests update state
handleTargetChanged _ _ state = pure state

handleClockTickUpdate :: (Action :> es, RetrieveEnvironment :> es) => PromotionTimeout -> UTCTime -> ProjectState -> Eff es ProjectState
handleClockTickUpdate (Config.PromotionTimeout timeout) currTime state = do
  let prsToPromote = Pr.filterPullRequestsBy Pr.awaitingPromotion state
  foldM update state prsToPromote
  where
    update state' prId = let pr = fromJust $ Pr.lookupPullRequest prId state
      in case Pr.promotionTime pr of
      Nothing -> pure state'
      Just time -> if Time.addTime time timeout < currTime
        then tryPromotePullRequest pr prId state'
        else pure state'

-- | Internal result type for parsing a merge command, which allows the
-- consumer of `parseMergeCommand` to inspect the reason why a message
-- was considered invalid.
data ParseResult a
  -- | The parser found a valid prefix and a valid command.
  = Success a
  -- | The parser found a valid prefix, but no valid command. This contains a
  -- (multiline) error message describing the problem with the command. When
  -- displaying the error in a GitHub comment, it should be shown as monospace
  -- text as it may use whitespace for alignment and it may also contain
  -- markdown from the original comment.
  | ParseError Text
  -- | The parser decided to ignore the message because it did
  -- not contain a valid prefix.
  | Ignored
  deriving (Show)

-- Checks whether the parse result was valid.
isSuccess :: ParseResult a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

type Parser = Parsec Void Text

-- | A comment that can be added to a message that will cause it to not parse
-- the message for merge commands. This is parsed in 'shouldIgnoreComment'. Note
-- the trailing line feed here. GitHub's markdown parser won't parse markdown if
-- it's on the same line as an HTML comment:
--
-- https://github.com/channable/hoff/issues/227
hoffIgnoreComment :: Text
hoffIgnoreComment = "<!-- Hoff: ignore -->\n"

-- | Checks if a comment contains 'hoffIgnoreComment', matching case
-- insensitively and allowing variations in whitespace. This is used to prevent
-- feedback cycles when Hoff repeats part of a message posted by the user. This
-- is parsed this way instead of simple string matching to be a bit more
-- reliable if users start using this as well.
shouldIgnoreComment :: Text -> Bool
shouldIgnoreComment = cvtParseResult . P.parse pComment "comment"
  where
    cvtParseResult :: Either (ParseErrorBundle Text Void) Bool -> Bool
    cvtParseResult (Right True) = True
    cvtParseResult _ = False

    -- Helper to parse a string, case insensitively, and ignoring excess spaces
    -- between words. Also allows line breaks.
    pString :: Text -> Parser ()
    pString = sequence_ . intersperse P.hspace1 . map (void . P.string) . Text.words

    pComment :: Parser Bool
    pComment = (True <$ pHoffIgnoreComment)
      <|> (P.anySingle *> pComment)
      <|> pure False

    -- This parses 'hoffIgnoreComment' while being as lenient in terms of
    -- whitespace usage and casing as possible.
    pHoffIgnoreComment :: Parser ()
    pHoffIgnoreComment = P.string "<!--" *> P.space *> pString "Hoff: ignore" <* P.space <* P.string "-->"

-- | Parse a PR comment for a merge command. The parsing is done
-- case-insensitively and duplicate whitespace is ignored. The 'ParseResult'
-- indicates whether:
--
--   1) The comment contains a properly parsed merge command.  To reduce
--      ambiguity, the command should be either on its own line or at the end of
--      a sentence (which may end with a period, comma, exclamation mark, or
--      question mark). If the line contains other text that cannot be parsed
--      then this is treated as an error.
--   2) The comment did contain the merge command's prefix but the text folowing
--      it was not a valid merge command according to the rules mentioned above.
--   3) The comment did not contain the command prefix at all and should be
--      ignored.
--
-- If the trigger prefix is "@hoffbot", a command "@hoffbot merge" would
-- indicate the `Merge` approval type.
parseMergeCommand :: ProjectConfiguration -> TriggerConfiguration -> Text -> ParseResult (MergeCommand, MergeWindow)
parseMergeCommand projectConfig triggerConfig = cvtParseResult . P.parse pComment "comment"
  where
    cvtParseResult :: Either (ParseErrorBundle Text Void) (Maybe a) -> ParseResult a
    cvtParseResult (Right (Just result)) = Success result
    cvtParseResult (Right Nothing) = Ignored
    cvtParseResult (Left err) = ParseError (Text.pack $ P.errorBundlePretty err)

    -- This parser maintains a lot of the odd parser's behavior in that in folds
    -- repeated whitespace into one, matches case insensitively, and strips the
    -- prefix (but not the environment names) before matching.
    commandPrefix :: Text
    commandPrefix = Text.strip $ Config.commentPrefix triggerConfig

    -- No whitespace stripping or case folding is performed here since they are
    -- also matched verbatim elsewhere in Hoff.
    environments :: [Text]
    environments = fromMaybe [] (deployEnvironments projectConfig)

    -- The punctuation characters that are allowed at the end of a merge
    -- command. This doesn't use the included punctuation predicate because that
    -- includes many more classes of punctuation, like quotes.
    allowedPunctuation :: [Char]
    allowedPunctuation = ".,!?:;"

    -- The error message printed when a comand is not terminated correctly.
    commentSuffixError :: String
    commentSuffixError = "Merge commands may not be followed by anything other than a punctuation character ("
      <> intercalate ", " (map show allowedPunctuation)
      <> ")."

    -- The error message printed when using 'merge and deploy' with no
    -- configured deployment environments.
    noDeployEnvironmentsError :: String
    noDeployEnvironmentsError = "No deployment environments have been configured."

    -- The error message printed when using 'merge and deploy' with no specified environment
    noDeployEnvironmentSpecifiedError :: String
    noDeployEnvironmentSpecifiedError = "Merge and deploy has been deprecated. Please use merge and deploy to <environment>\n" ++
      "where <environment> is one of " ++ intercalate ", " (map Text.unpack environments)

    -- Helper to parse a string, case insensitively, and ignoring excess spaces
    -- between words.
    pString :: Text -> Parser ()
    pString = sequence_ . intersperse P.hspace1 . map (void . P.string') . Text.words

    -- This parser succeeds if it either successfully parses the comment for a
    -- merge command, in which case it returns @Just (approval, mergeWindow)@,
    -- or when the comment doesn't contain any merge command, in which case the
    -- parser returns @Nothing@. The prefix is matched greedily in 'pCommand',
    -- so if the comment contains an invaild command followed by a valid command
    -- an error will be returned based on that earlier invalid command.
    pComment :: Parser (Maybe (MergeCommand, MergeWindow))
    pComment = (Just <$> pCommand)
      <|> (P.anySingle *> pComment)
      <|> pure Nothing

    -- Parse a full merge command. Does not consume any input if the prefix
    -- could not be matched fully.
    pCommand :: Parser (MergeCommand, MergeWindow)
    pCommand = P.try pCommandPrefix *> P.hspace1 *> (pApprovalCommand <|> pRetryCommand) <* P.hspace <* pCommandSuffix

    -- Parse the (normalized) command prefix. Matched non-greedily in 'pCommand'
    -- using 'P.try'.
    pCommandPrefix :: Parser ()
    pCommandPrefix = void $ P.string' commandPrefix

    -- Commands may be terminated by one or more (common) punctuation
    -- characters, one or more whitespace characters, and either the end of a
    -- line or the end of the input.
    pCommandSuffix :: Parser ()
    pCommandSuffix =
      P.many (P.oneOf allowedPunctuation)
      *> P.hspace
      *> (void P.eol <|> P.eof <|> fail commentSuffixError)

    -- Parse the actual merge approval command following the command prefix. The
    -- merge window is either @ on friday@ or empty.
    --
    -- NOTE: Since @ on friday@ starts with a space, additional whitespace at
    --       the end of 'pMergeApproval' should not already have been consumed.
    --       This is a bit tricky, and using 'P.hspace' instead of 'P.hspace1'
    --       in 'pMergeWindow' would allow @mergeon friday@ which is also not
    --       desirable.
    pApprovalCommand :: Parser (MergeCommand, MergeWindow)
    pApprovalCommand = (,) . Approve <$> pMergeApproval <*> pMergeWindow

    pRetryCommand :: Parser (MergeCommand, MergeWindow)
    pRetryCommand = (Retry,) <$> (P.string' "retry" *> pMergeWindow)

    -- We'll avoid unnecessary backtracking here by parsing the common prefixes.
    -- Note that 'P.try' is used sparingly here. It's mostly used when parsing
    -- whitespace plus another word. Backtracking should be limited to trying
    -- difference branches since it will otherwise destroy the nice error
    -- messages megaparsec gives us, and the parser will instead error out in
    -- 'pCommandSuffix' which would be confusing.
    --
    -- When the comment isn't folowed by @ and @ this is treated as a plain
    -- merge command.
    pMergeApproval :: Parser ApprovedFor
    pMergeApproval = pString "merge" *> (pMergeAnd <|> pure Merge)

    -- NOTE: As mentioned above, only the @ and @ part will backtrack. This is
    --       needed so a) the custom error message in pDeploy works and b) so
    --       'merge on friday' can be parsed correctly.
    pMergeAnd :: Parser ApprovedFor
    pMergeAnd = P.try (P.hspace1 *> pString "and" *> P.hspace1) *> (pTag <|> pDeploy)

    -- Parses @merge and tag@ commands.
    pTag :: Parser ApprovedFor
    pTag = MergeAndTag <$ pString "tag"

    -- Parses @merge and deploy[ to <environment>]@ commands.
    pDeploy :: Parser ApprovedFor
    pDeploy = MergeAndDeploy <$> (pString "deploy" *> pDeployToEnvironment)

    -- This parser is run directly after parsing "deploy", so it may need to
    -- parse a space character first since specifying a deployment environment
    -- is optional. The reason for splitting this up from 'pDeploy' like this is
    -- so we can have a nicer error message when no environments have been
    -- configured.
    pDeployToEnvironment :: Parser DeployEnvironment
    pDeployToEnvironment =
      if null environments
        then fail noDeployEnvironmentsError
        -- Without the try this could consume the space and break 'merge and deploy on friday'
        else P.try (P.hspace1 *> pString "to" *> P.hspace1 *> P.choice pDeployEnvironments)
               <|> defaultEnvironment

    -- The default environment to deploy to on a "merge and deploy". This
    -- behavior is deprecated when more than one environment is set, and will
    -- return an error in that case.
    defaultEnvironment :: Parser DeployEnvironment
    defaultEnvironment
      | [environment] <- environments = pure (DeployEnvironment environment)
      | otherwise = fail noDeployEnvironmentSpecifiedError

    -- NOTE: This uses 'P.string' instead of 'P.string'' to avoid case folding,
    --       since environment names are also matched case sensitively elsewhere
    pDeployEnvironments :: [Parser DeployEnvironment]
    pDeployEnvironments = map (fmap DeployEnvironment . P.string) environments

    -- Parses the optional @ on friday@ command suffix. Since this starts with a
    -- space, it's important that the last run parser has not yet consumed it.
    pMergeWindow :: Parser MergeWindow
    pMergeWindow = (OnFriday <$ P.try (P.hspace1 *> pString "on friday")) <|> (DuringFeatureFreeze <$ P.try (P.hspace1 *> pString "as hotfix")) <|> pure AnyDay

-- Mark the pull request as approved, and leave a comment to acknowledge that.
approvePullRequest :: PullRequestId -> Approval -> ProjectState -> Eff es ProjectState
approvePullRequest pr approval = pure . Pr.updatePullRequest pr
    (\pullRequest -> pullRequest
      { Pr.approval = Just approval
      , Pr.needsFeedback = True
      })

handleCommentAdded
  :: forall es. (Action :> es, RetrieveEnvironment :> es)
  => TriggerConfiguration
  -> MergeWindowExemptionConfiguration
  -> Maybe FeatureFreezeWindow
  -> PullRequestId
  -> Username
  -> Text
  -> ProjectState
  -> Eff es ProjectState
handleCommentAdded triggerConfig mergeWindowExemption featureFreezeWindow prId author body state
  -- Parser error messages contain an excerpt from the original's comment. To
  -- avoid feedback loops, Hoff will insert a special comment into its own
  -- comments with parser error messages that can be checked for here.
  | shouldIgnoreComment body = pure state
  | otherwise =
  let maybePR = Pr.lookupPullRequest prId state in
  case maybePR of
    -- Check if the comment is a merge command, and if it is, check if the
    -- author is allowed to approve. Comments by users with push access happen
    -- frequently, but most comments are not merge commands, and checking that
    -- a user has push access requires an API call.
    Just pr -> do
      projectConfig <- getProjectConfig

      let commandType = parseMergeCommand projectConfig triggerConfig body
      -- Check whether the author is allowed to do merge commands, but only if
      -- a valid command was parsed.
      isAllowed <- if isSuccess commandType
        then isReviewer author
        else pure False

      dateTime <- getDateTime

      -- To guard against accidental merges we make use of a merge window.
      -- Merging inside this window is discouraged but can be overruled with a
      -- special command or by adding the user to the merge window exemption
      -- list. For now Friday at UTC+0 is good enough. See
      -- https://github.com/channable/hoff/pull/95 for caveats and improvement
      -- ideas.
      let day = dayOfWeek $ utctDay $ dateTime

      let featureFreezeActive = case featureFreezeWindow of
            Nothing -> False
            Just (Config.FeatureFreezeWindow start end) -> dateTime > start && dateTime < end

      let exempted :: Username -> Bool
          exempted (Username user) =
            let (Config.MergeWindowExemptionConfiguration users) = mergeWindowExemption
            in elem user users

          verifyMergeWindow :: MergeCommand -> MergeWindow -> Eff es ProjectState -> Eff es ProjectState
          verifyMergeWindow _ _ action | exempted author = action
          verifyMergeWindow command DuringFeatureFreeze action
            | featureFreezeActive = action
            | otherwise = do
                () <- leaveComment prId ("Your merge request has been denied because \
                                          \it is not a feature-freeze period. Run '" <>
                                          Pr.displayMergeCommand command <> "' instead.")
                pure state
          verifyMergeWindow command OnFriday action
            | day == Friday = action
            | otherwise = do
                () <- leaveComment prId ("Your merge request has been denied because \
                                          \it is not Friday. Run '" <>
                                          Pr.displayMergeCommand command <> "' instead.")
                pure state
          verifyMergeWindow command AnyDay action
            | featureFreezeActive = do
                () <- leaveComment prId ("Your merge request has been denied because \
                                          \we are in a feature-freeze period. Run '" <>
                                          Pr.displayMergeCommand command <> " as hotfix' instead.")
                pure state
            | day == Friday = do
                () <- leaveComment prId ("Your merge request has been denied, because \
                                          \merging on Fridays is not recommended. \
                                          \To override this behaviour use the command `"
                                          <> Pr.displayMergeCommand command <> " on Friday`.")
                pure state
            | otherwise = action

      case commandType of
        -- The bot was not mentioned in the comment, ignore
        Ignored -> pure state
        -- The bot was mentioned but encountered an invalid command, report error and
        -- take no further action
        ParseError message -> do
          -- The parser error message may use whitespace for alignment and it
          -- may also contain markdown from the original comment. It should thus
          -- be formatted as monospace text so it displays correctly. This uses
          -- the oldschool four space markdown code blocks instead of fenced
          -- code blocks since it's less ambiguous.
          let monospaceMessage = Text.unlines . map ("    " <>) . Text.lines $ message
              -- NOTE: This comment is added to prevent feedback loops, see
              --       'shouldIgnoreComment'
              fullComment = hoffIgnoreComment <> "Unknown or invalid command found:\n\n" <> monospaceMessage
          () <- leaveComment prId fullComment
          pure state
        -- Cases where the parse was successful
        Success (command, mergeWindow)
          -- Author is a reviewer
          | isAllowed -> verifyMergeWindow command mergeWindow $ case command of
            Approve approval -> handleMergeRequested projectConfig prId author state pr approval Nothing
            Retry -> handleMergeRetry projectConfig prId author state pr
          -- Author is not a reviewer, so we ignore
          | otherwise -> pure state
    -- If the pull request is not in the state, ignore the comment.
    Nothing -> pure state

doMerge
  :: ProjectConfiguration
  -> PullRequestId
  -> Username
  -> ProjectState
  -> PullRequest
  -> ApprovedFor
  -> Maybe Username
  -> Eff es ProjectState
doMerge projectConfig prId author state pr approvalType retriedBy = do
  let (order, state') = Pr.newApprovalOrder state
  state'' <- approvePullRequest prId (Approval author approvalType order retriedBy) state'
  -- Check whether the integration branch is valid, if not, mark the integration as invalid.
  if Pr.baseBranch pr /= BaseBranch (Config.branch projectConfig)
    then pure $ Pr.setIntegrationStatus prId IncorrectBaseBranch state''
    else pure state''

-- | Someone issued a `merge*` command on a PR. Depending on what the current
-- integration state of that PR is, we might reset its state and retry, if it
-- already exists.
handleMergeRequested
  :: (Action :> es)
  => ProjectConfiguration
  -> PullRequestId
  -> Username
  -> ProjectState
  -> PullRequest
  -> ApprovedFor
  -> Maybe Username
  -> Eff es ProjectState
handleMergeRequested projectConfig prId author state pr approvedFor retriedBy
  = case Pr.integrationStatus pr of
      NotIntegrated -> doMerge projectConfig prId author state pr approvedFor retriedBy
      Integrated _ checks | not (Pr.isFinalStatus (summarize checks)) -> do
        state' <- clearPullRequest prId pr state
        doMerge projectConfig prId author state' pr approvedFor retriedBy
      Conflicted _ _ ->
        leaveComment prId "Conflict encountered while integrating, refusing..." >> pure state
      IncorrectBaseBranch -> do
        leaveComment prId "Incorrect base branch, refusing..." >> pure state
      _ ->
        -- In any other case, we hit a final state, `Promoted`, `Integrated BuildSucceeded`
        -- or `Integrated BuildFailed`. Report the current state.
        pure $ Pr.setNeedsFeedback prId True state

-- | Attempt to retry merging a PR that has previously been approved for
-- merging.
handleMergeRetry
  :: (Action :> es)
  => ProjectConfiguration
  -> PullRequestId
  -> Username
  -> ProjectState
  -> PullRequest
  -> Eff es ProjectState
handleMergeRetry projectConfig prId author state pr
  -- Only approved PRs with failed builds can be retried
  | Just approval <- Pr.approval pr,
    Integrated _ buildStatus <- Pr.integrationStatus pr,
    BuildFailed{} <- summarize buildStatus = do
      state' <- clearPullRequest prId pr state
      -- The PR is still approved by its original approver. The person who
      -- triggered the retry is tracked separately.
      doMerge projectConfig prId (Pr.approver approval) state' pr (Pr.approvedFor approval) (Just author)
  | otherwise = do
      () <- leaveComment prId "Only approved PRs with failed builds can be retried.."
      pure state

-- | Given a pull request id, mark all pull requests that follow from it
--   in the merge train as NotIntegrated
unintegrateAfter :: PullRequestId -> ProjectState -> ProjectState
unintegrateAfter pid state = case Pr.lookupPullRequest pid state of
  Nothing -> state -- PR not found.  Keep the state as it is.
  Just pr -> unintegrateAfter' pr state
  where
  unintegrateAfter' :: PullRequest -> ProjectState -> ProjectState
  unintegrateAfter' pr0 = Pr.updatePullRequests unintegrate
    where
    unintegrate pr | pr `Pr.approvedAfter` pr0 && Pr.isIntegratedOrSpeculativelyConflicted pr
                   = pr{Pr.integrationStatus = NotIntegrated}
                   | otherwise
                   = pr

-- | If there is an integration candidate, and its integration sha matches that of the build,
--   then update the build status for that pull request. Otherwise do nothing.
handleBuildStatusChanged :: Sha -> Context -> BuildStatus -> ProjectState -> Eff es ProjectState
handleBuildStatusChanged buildSha context newStatus state = pure $
  compose [ unintegratePullRequestIfNeeded pid
          . Pr.updatePullRequest pid setBuildStatus
          | pid <- Pr.filterPullRequestsBy shouldUpdate state
          ] state
  where
  satisfiedCheck = contextSatisfiesChecks (Pr.mandatoryChecks state) context
  getNewStatus new old = if new `supersedes` old then new else old
  newStatusState oldStatus = case oldStatus of
    Pr.AnyCheck old -> Pr.AnyCheck $ getNewStatus newStatus old
    Pr.SpecificChecks checks -> case satisfiedCheck of
      Just check -> Pr.SpecificChecks $ Map.insertWith getNewStatus check newStatus checks
      -- Ignore status updates that aren't relevant to the mandatory checks
      Nothing -> Pr.SpecificChecks checks

  shouldUpdate pr = case Pr.integrationStatus pr of
    Integrated candidateSha _ -> candidateSha == buildSha
    _                         -> False

  -- We need to do edge detection for failures on the summarized status of the
  -- pull request, as we only want to trigger unintegration once. The nature of
  -- webhooks make arrival guarentees annoying to deal with, so we opt for
  -- only dealing with the first appearance of a status.
  unintegratePullRequestIfNeeded pid newState
    | Just oldPr <- Pr.lookupPullRequest pid state
    , Just newPr <- Pr.lookupPullRequest pid newState
    , Integrated _ oldStatus <- Pr.integrationStatus oldPr
    , Integrated _ newStatus' <- Pr.integrationStatus newPr
    = let summarized = summarize newStatus' in if
        | summarized `supersedes` summarize oldStatus
        , BuildFailed _ <- summarized -> unintegrateAfter pid newState
        | otherwise -> newState
  unintegratePullRequestIfNeeded _ newState = newState

  -- Like unintegration, we also need edge detection to avoid commenting
  -- multiple times on the same PR.
  setBuildStatus pr
    | Integrated _ oldStatus <- Pr.integrationStatus pr
    = let newStatus' = newStatusState oldStatus
          wasSuperseded = summarize newStatus' `supersedes` summarize oldStatus
      in pr
        { Pr.integrationStatus = Integrated buildSha newStatus'
        , Pr.needsFeedback = case newStatus of
                              BuildStarted _ -> wasSuperseded
                              BuildFailed _  -> wasSuperseded
                              _              -> Pr.needsFeedback pr -- unchanged
        }
    | otherwise = pr

-- | Check if the given context for a status update pertains to any of the known
-- mandatory checks for the project.
contextSatisfiesChecks :: Pr.MandatoryChecks -> Git.Context -> Maybe Project.Check
contextSatisfiesChecks (Pr.MandatoryChecks checks) (Git.Context context) =
  let go []                           = Nothing
      go (c@(Project.Check check):cs) = if check `Text.isPrefixOf` context
        then Just c
        else go cs
  in  go (Set.toList checks)

-- Query the GitHub API to resolve inconsistencies between our state and GitHub.
synchronizeState :: Action :> es => ProjectState -> Eff es ProjectState
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

-- | Determines if there is anything to do, and if there is, generates the right
-- actions and updates the state accordingly.
--
-- For example, if the current integration candidate has been integrated
-- (and is no longer a candidate), we should find a new candidate. Or after the
-- pull request for which a build is in progress is closed, we should find a
-- new candidate.
--
-- This is called repeatedly from 'proceedUntilFixedPoint' until the state is
-- unchanged.
--
-- This function prioritizes actions in the following order:
--
-- 1. providing feedback through comments
--    ('provideFeedback');
--
-- 2. proceeding with the first candidate, i.e.: the head of the train
--    ('proceedFirstCandidate') including promoting it to @ master @;
--
-- 3. trying to integrate a new pull request in the train
--    ('tryIntegrateFillPullRequest').
--
-- Often actions performed here are done in multiple passes.
--
-- For example, if a build fails, this is recorded in the project/repository
-- status and the 'needsFeedback' flag is set to 'True'.  Only in the second
-- pass the failure will be reported to the user.  Since this function is
-- always called from 'proceedUntilFixedPoint', this happens as a single
-- action.
proceed
  :: (Action :> es, RetrieveEnvironment :> es, TimeOperation :> es)
  => ProjectState
  -> Eff es ProjectState
proceed = provideFeedback
      >=> proceedFirstCandidate
      >=> tryIntegrateFirstPullRequest

-- | Proceeds with the candidate that was approved first
-- by pushing it to be the new master if the build succeeded.
-- (cf. 'proceedCandidate')
proceedFirstCandidate
  :: (Action :> es, RetrieveEnvironment :> es, TimeOperation :> es)
  => ProjectState
  -> Eff es ProjectState
proceedFirstCandidate state = case Pr.unfailedIntegratedPullRequests state of
  (candidate:_) -> proceedCandidate candidate state
  _ -> pure state

-- | Try to integrate the pull request that was approved first
-- if there's one.
-- (cf. 'tryIntegratePullRequest')
tryIntegrateFirstPullRequest :: Action :> es => ProjectState -> Eff es ProjectState
tryIntegrateFirstPullRequest state = case Pr.candidatePullRequests state of
  (pr:_) -> tryIntegratePullRequest pr state
  _ -> pure state

-- | Pushes the given integrated PR to be the new master if the build succeeded
proceedCandidate
  :: (Action :> es, RetrieveEnvironment :> es, TimeOperation :> es)
  => PullRequestId
  -> ProjectState
  -> Eff es ProjectState
proceedCandidate pullRequestId state =
  case Pr.lookupPullRequest pullRequestId state of
  Nothing -> pure state -- should not be reachable when called from 'proceed'
  Just pullRequest ->
    case Pr.integrationStatus pullRequest of
      -- Check if all mandatory checks succeeded for the given sha and push
      -- the candidate if so.
      Integrated sha (summarize -> BuildSucceeded) ->
        pushCandidate (pullRequestId, pullRequest) sha state
      _ ->
        pure state -- BuildFailed/BuildPending

-- Given a pull request id, returns the name of the GitHub ref for that pull
-- request, so it can be fetched.
getPullRequestRef :: PullRequestId -> Branch
getPullRequestRef (PullRequestId n) = Branch $ format "refs/pull/{}/head" [n]

-- Integrates proposed changes from the pull request into the target branch.
-- The pull request must exist in the project.
tryIntegratePullRequest :: Action :> es => PullRequestId -> ProjectState -> Eff es ProjectState
tryIntegratePullRequest pr state =
  let
    PullRequestId prNumber = pr
    pullRequest  = fromJust $ Pr.lookupPullRequest pr state
    title = Pr.title pullRequest
    Approval (Username approvedBy) approvalType _prOrder _retriedBy = fromJust $ Pr.approval pullRequest
    candidateSha = Pr.sha pullRequest
    candidateRef = getPullRequestRef pr
    candidate = (pr, candidateRef, candidateSha)
    mergeMessageLines =
      [ format "Merge #{}: {}" (prNumber, title)
      , ""
      , format "Approved-by: {}" [approvedBy]
      ] ++
        case approvalType of
          MergeAndDeploy (DeployEnvironment env) ->
            [ "Auto-deploy: true"
            , format "Deploy-Environment: {}" [env]]
          _ -> [ "Auto-deploy: false" ]

    mergeMessage = Text.unlines mergeMessageLines
    -- the takeWhile here is needed in case of reintegrations after failing pushes
    train = takeWhile (/= pr) $ Pr.unfailedIntegratedPullRequests state
  in do
    result <- tryIntegrate mergeMessage candidate train $ Pr.alwaysAddMergeCommit approvalType
    case result of
      Left (IntegrationFailure targetBranch reason) ->
        -- If integrating failed, perform no further actions but do set the
        -- state to conflicted.
        -- If this is a speculative rebase, we wait before giving feedback.
        -- For WrongFixups, we can report issues right away.
        pure $ Pr.setIntegrationStatus pr (Conflicted targetBranch reason) $
          Pr.setNeedsFeedback pr (null train || reason == WrongFixups) state

      Right (Sha sha) -> do
        -- If it succeeded, set the build to pending,
        -- as pushing should have triggered a build.
        let Pr.MandatoryChecks check = Pr.mandatoryChecks state
            integrationStatus = if Set.null check
              then Pr.AnyCheck BuildPending
              else Pr.SpecificChecks (Map.fromSet (const BuildPending) check)

        pure
          -- The build pending has no URL here, we need to wait for semaphore
          $ Pr.setIntegrationStatus pr (Integrated (Sha sha) integrationStatus)
          $ Pr.setNeedsFeedback pr True
          $ state

-- Pushes the integrated commits of the given candidate pull request to the
-- target branch. If the push fails, restarts the integration cycle for the
-- candidate.
-- TODO: Get rid of the tuple; just pass the ID and do the lookup with fromJust.
pushCandidate
  :: (Action :> es, RetrieveEnvironment :> es, TimeOperation :> es)
  => (PullRequestId, PullRequest)
  -> Sha
  -> ProjectState
  -> Eff es ProjectState
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
    currTime <- Time.getDateTime
    (pushResult, newStatus) <- if Pr.needsTag approvalKind
        then do
          versionOrBadTag <- getLatestVersion newHead
          case versionOrBadTag of
            Left (TagName bad) -> do
              commentToUser ("Sorry, I could not tag your PR. The previous tag `" <> bad <> "` seems invalid")
              pushResult <- tryForcePush prBranch newHead
              pure (pushResult, Pr.Promote currTime newHead)
            Right v -> do
              let previousTag = versionToTag v
              mChangelog <- getChangelog previousTag newHead
              let
                tagName = versionToTag $ v + 1
                changelog = fromMaybe "Failed to get the changelog" mChangelog
                tagMessage = messageForTag tagName approvalKind changelog
              pushResult <- tryForcePush prBranch newHead
              pure (pushResult, Pr.PromoteAndTag currTime newHead tagName tagMessage)
      else do
        pushResult <- tryForcePush prBranch newHead
        pure (pushResult, Pr.Promote currTime newHead)
    case pushResult of
      PushOk -> pure $ Pr.setIntegrationStatus pullRequestId newStatus state
      PushRejected _ -> tryIntegratePullRequest pullRequestId
                      $ unintegrateAfter pullRequestId state

-- | When a pull request has been promoted to master this means that any
-- conflicts (failed rebases) built on top of it are not speculative anymore:
-- they are real conflicts on top of the (new) master.
--
-- This function updates the conflicted bases for all pull requests that come
-- after the given PR and sets them to need feedback.
unspeculateConflictsAfter :: PullRequest -> PullRequest -> PullRequest
unspeculateConflictsAfter promotedPullRequest pr
  | Pr.PullRequest{ Pr.integrationStatus = Conflicted specBase reason
                  , Pr.baseBranch        = realBase
                  } <- pr
  , specBase /= realBase && pr `Pr.approvedAfter` promotedPullRequest
  = pr { Pr.integrationStatus = Conflicted realBase reason
       , Pr.needsFeedback = True
       }
  | otherwise
  = pr

-- | When a pull request has been promoted to master this means that any build
-- failures build on top of it are not speculative anymore: they are real build
-- failures on top of the (new) master.
--
-- This function simply sets them to be sent feedback again this time the build
-- failure will be reported as a real definitive failure.
unspeculateFailuresAfter :: PullRequest -> PullRequest -> PullRequest
unspeculateFailuresAfter promotedPullRequest pr
  | Pr.PullRequest{Pr.integrationStatus = Integrated _ (summarize -> BuildFailed _)} <- pr
  , pr `Pr.approvedAfter` promotedPullRequest
  = pr{Pr.needsFeedback = True}
  | otherwise
  = pr

-- | Keep doing a 'proceed' step until the state doesn't change any more.
-- For this to work properly, it is essential that 'proceed' does not have any
-- side effects if it does not change the state.
proceedUntilFixedPoint
  :: (Action :> es, RetrieveEnvironment :> es, TimeOperation :> es)
  => ProjectState
  -> Eff es ProjectState
proceedUntilFixedPoint state = do
  newState <- proceed state
  if newState == state
    then return state
    else proceedUntilFixedPoint newState

-- Describe the status of the pull request.
describeStatus :: BaseBranch -> PullRequestId -> PullRequest -> ProjectState -> Text
describeStatus (BaseBranch projectBaseBranchName) prId pr state = case Pr.classifyPullRequest pr of
  PrStatusAwaitingApproval -> "Pull request awaiting approval."
  PrStatusApproved ->
    let
      Approval (Username approvedBy) approvalType _position retriedBy = fromJust $ Pr.approval pr

      approvalCommand = Pr.displayMergeCommand (Approve approvalType)
      retriedByMsg = case retriedBy of
        Just user -> format " (retried by @{})" [user]
        Nothing -> mempty
      queuePositionMsg = case Pr.getQueuePosition prId state of
        0 -> "rebasing now"
        1 -> "waiting for rebase behind one pull request"
        n -> format "waiting for rebase behind {} pull requests" [n]
    in format "Pull request approved for {} by @{}{}, {}." [approvalCommand, approvedBy, retriedByMsg, queuePositionMsg]
  PrStatusOutdated ->
    let BaseBranch baseBranchName = Pr.baseBranch pr
    in format "Push to {} detected, rebasing again." [baseBranchName]
  PrStatusBuildPending -> let Sha sha = fromJust $ Pr.integrationSha pr
                              train   = takeWhile (/= prId) $ Pr.unfailedIntegratedPullRequests state
                              len     = length train
                              prs     = if len == 1 then "PR" else "PRs"
                          in case train of
                             []    -> Text.concat ["Rebased as ", sha, ", waiting for CI …"]
                             (_:_) -> Text.concat [ "Speculatively rebased as ", sha
                                                  , " behind ", Text.pack $ show len
                                                  , " other ", prs
                                                  , ", waiting for CI …"
                                                  ]
  PrStatusBuildStarted url -> Text.concat ["[CI job :yellow_circle:](", url, ") started."]
  PrStatusAwaitingPromotion -> "The PR is waiting to be pushed to the target branch"
  PrStatusIntegrated -> "The build succeeded."
  PrStatusIncorrectBaseBranch ->
    let BaseBranch baseBranchName = Pr.baseBranch pr
    in format "Merge rejected: the base branch of this pull request must be set to {}. It is currently set to {}."
              [projectBaseBranchName, baseBranchName]
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
      , "    git fetch && git rebase --interactive --autosquash origin/"
      , targetBranchName
      , " "
      , prBranchName
      ]
  -- The following is not actually shown to the user
  -- as it is never set with needsFeedback=True,
  -- but here in case we decide to show it.
  PrStatusSpeculativeConflict -> "Failed to speculatively rebase. \
                                 \ I will retry rebasing automatically when the queue clears."
  PrStatusFailedBuild url -> case Pr.unfailedIntegratedPullRequestsBefore pr state of
    -- On Fridays the retry command is also `retry on friday`. We currently
    -- don't have that information here. Is that worth including?
    [] -> format "The {}.\n\n\
                 \If this is the result of a flaky test, \
                 \then tag me again with the `retry` command.  \
                 \Otherwise, push a new commit and tag me again."
                 [markdownLink "build failed :x:" url]
    trainBefore -> format "Speculative {}. \
                          \ I will automatically retry after getting build results for {}."
                          [ markdownLink "build failed :x:" url
                          , prettyPullRequestIds trainBefore ]

-- Leave a comment with the feedback from 'describeStatus' and set the
-- 'needsFeedback' flag to 'False'.
leaveFeedback
  :: (Action :> es, RetrieveEnvironment :> es)
  => (PullRequestId, PullRequest)
  -> ProjectState
  -> Eff es ProjectState
leaveFeedback (prId, pr) state = do
  projectBaseBranch <- getBaseBranch
  let message = describeStatus projectBaseBranch prId pr state
  -- Hoff shouldn't reply to any of its own feedback messages. This can happen
  -- if external automation causes the bot to issue a merge command to itself.
  -- In that case the bot may tag itself when the merge gets approved.
  () <- leaveComment prId $ hoffIgnoreComment <> message
  pure $ Pr.setNeedsFeedback prId False state

-- Run 'leaveFeedback' on all pull requests that need feedback.
provideFeedback
  :: (Action :> es, RetrieveEnvironment :> es)
  => ProjectState
  -> Eff es ProjectState
provideFeedback state
  = foldM (flip leaveFeedback) state
  $ filter (Pr.needsFeedback . snd)
  $ fmap (first PullRequestId)
  $ IntMap.toList $ Pr.pullRequests state

handleEvent
  :: (Action :> es, RetrieveEnvironment :> es, TimeOperation :> es)
  => TriggerConfiguration
  -> MergeWindowExemptionConfiguration
  -> Maybe FeatureFreezeWindow
  -> PromotionTimeout
  -> Event
  -> ProjectState
  -> Eff es ProjectState
handleEvent triggerConfig mergeWindowExemption featureFreezeWindow promotionTimeout event state = do
  projectState <- handleEventInternal triggerConfig mergeWindowExemption featureFreezeWindow promotionTimeout event state >>= proceedUntilFixedPoint
  triggerTrainSizeUpdate projectState
  pure projectState


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

pullRequestIdToText :: PullRequestId -> Text
pullRequestIdToText (PullRequestId prid) = Text.pack $ show prid

testBranch :: ProjectConfiguration -> PullRequestId -> Git.Branch
testBranch config pullRequestId = Git.Branch $ Config.testBranch config <> "/" <> pullRequestIdToText pullRequestId

-- | Textual rendering of a list of 'PullRequestId's
--
-- >>> prettyPullRequestIds [PullRequestId 12, PullRequestId 60, PullRequestId 1337]
-- "#12, #60 and #1337"
prettyPullRequestIds :: [PullRequestId] -> Text
prettyPullRequestIds = commaAnd . map prettyPullRequestId
  where
  prettyPullRequestId (PullRequestId n) = "#" <> Text.pack (show n)

-- | Pretty printing of a list of Text with comma and and.
--
-- >>> commaAnd ["a", "b", "c" :: Text]
-- "a, b and c"
commaAnd :: [Text] -> Text
commaAnd [] = "none"
commaAnd ss = case init ss of
              [] -> last ss
              is -> Text.intercalate ", " is <> " and " <> last ss

-- | Fold a list of unary functions by composition
--
-- Writing
--
-- > compose [f,g,h]
--
-- translates to
--
-- > f . g . h
compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- | Formats a markdown link in the presence of an URL
--
-- >>> markdownLink "build failed" Nothing
-- "build failed"
--
-- >>> markdownLink "build failed" (Just "https://example.com")
-- "[build failed](https://example.com)"
markdownLink :: Text -> Maybe Text -> Text
markdownLink text Nothing    = text
markdownLink text (Just url) = format "[{}]({})" [text, url]
