-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Project (
  Approval (..),
  ApprovedFor (..),
  MergeCommand (..),
  BuildStatus (..),
  DeployEnvironment (..),
  DeploySubprojects (..),
  MandatoryChecks (..),
  Check (..),
  IntegrationStatus (..),
  OutstandingChecks (..),
  Priority (..),
  ProjectInfo (..),
  ProjectState (..),
  PromotedPullRequest (..),
  PullRequest (..),
  PullRequestId (..),
  PullRequestStatus (..),
  Owner,
  unintegrateAfter,
  speculativelyFailedPullRequests,
  candidatePullRequests,
  classifyPullRequest,
  deletePullRequest,
  emptyProjectState,
  isInProgress,
  existsPullRequest,
  getQueuePosition,
  insertPullRequest,
  integrationSha,
  pause,
  resume,
  promotionSha,
  promotionTime,
  awaitingPromotion,
  integrationShas,
  loadProjectState,
  lookupPullRequest,
  saveProjectState,
  needsDeploy,
  isIntegratedOrSpeculativelyConflicted,
  needsTag,
  displayMergeCommand,
  addApproval,
  approve,
  removeFromTrain,
  trainPullRequests,
  trainBefore,
  sortedPullRequests,
  setIntegrationStatus,
  setNeedsFeedback,
  updatePullRequest,
  updatePullRequests,
  promotePullRequest,
  filterRecentlyPromoted,
  wasIntegrationAttemptFor,
  filterPullRequestsBy,
  isIntegrated,
  isFailedIntegrated,
  isUnfailedIntegrated,
  subMapByOwner,
  supersedes,
  summarize,
  isFinalStatus,
  MergeWindow (..),
)
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, (.!=), (.:), (.:?))
import Data.ByteString (readFile)
import Data.ByteString.Lazy (writeFile)
import Data.Foldable (asum, foldl')
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)
import Format (format)
import GHC.Generics
import Git (BaseBranch (..), Branch (..), GitIntegrationFailure (..), Sha (..), TagMessage, TagName)
import System.Directory (renameFile)
import Prelude hiding (readFile, writeFile)

import Data.Text.Buildable (Buildable (build))
import Data.Text.Lazy.Builder as Text

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Data.Time (UTCTime (..))
import Types (PullRequestId (..), ReactableId, Username)

-- For any integrated sha, we either wait for the first check, or for
-- a variety of mandatory checks to pass before merging the pullrequest.
data OutstandingChecks
  = AnyCheck BuildStatus
  | SpecificChecks (Map Check BuildStatus)
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The build status of a pull request.
data BuildStatus
  = -- | we have just pushed to @testing/id@
    BuildPending
  | -- | the build started with the given URL
    BuildStarted Text
  | -- | the build succeeded
    BuildSucceeded
  | -- | the build failed with the given URL
    BuildFailed (Maybe Text)
  deriving (Eq, Show, Generic)

-- | When attempting to integrated changes, there can be six states:
--
-- * no attempt has been made to integrate;
--
-- * the previous integration attempt became outdated by a push to the target branch.
--
-- * integration (e.g. merge or rebase) was successful
--   and the new commit has the given sha;
--
-- * the PR has been promoted to be the new master;
--
-- * and an attempt to integrate was made, but it wasn't successful.
--
-- * the base branch is incorrect.
data IntegrationStatus
  = NotIntegrated
  | Outdated
  | Integrated Sha OutstandingChecks
  | Promote UTCTime Sha
  | PromoteAndTag UTCTime Sha TagName TagMessage
  | Promoted
  | Conflicted BaseBranch GitIntegrationFailure
  | IncorrectBaseBranch
  deriving (Eq, Show, Generic)

data PullRequestStatus
  = -- | New, awaiting review.
    PrStatusAwaitingApproval
  | -- | Approved, but not yet integrated or built.
    PrStatusApproved
  | -- | Approved, but master updated during integration.
    PrStatusOutdated
  | -- | Integrated, and build pending or in progress.
    PrStatusBuildPending
  | -- | Integrated, and build pending or in progress.
    PrStatusBuildStarted Text
  | -- | Integrated, waiting to be pushed to target branch
    PrStatusAwaitingPromotion
  | -- | Integrated, build passed, merged into target branch.
    PrStatusIntegrated
  | -- | Integration branch not being valid.
    PrStatusIncorrectBaseBranch
  | -- | Failed to integrate due to the presence of orphan fixup commits.
    PrStatusWrongFixups
  | -- | Rebase was empty (changes already in the target branch?)
    PrStatusEmptyRebase
  | -- | Failed to integrate due to merge conflict.
    PrStatusFailedConflict
  | -- | Failed to integrate but this was a speculative build
    PrStatusSpeculativeConflict
  | -- | Integrated, but the build failed.
    --   Field should contain the URL to a page
    --   explaining the build failure.
    PrStatusFailedBuild (Maybe Text)
  deriving (Eq, Show)

newtype DeployEnvironment = DeployEnvironment Text
  deriving (Eq, Show, Generic)

data DeploySubprojects
  = EntireProject
  | OnlySubprojects [Text]
  deriving (Eq, Show, Generic)

-- | A PR can be approved to be merged with "<prefix> merge", or it can be
-- approved to be merged and also deployed with "<prefix> merge and deploy".
-- This enumeration distinguishes these cases.
data ApprovedFor
  = Merge
  | MergeWithoutDeploy
  | MergeAndDeploy DeploySubprojects DeployEnvironment
  | MergeAndTag
  deriving (Eq, Show, Generic)

-- | A command parsed from a comment in @Logic.parseMergeCommand@.
data MergeCommand
  = -- | The PR should be approved for merging and optionally deploying.
    Approve ApprovedFor
  | -- | Retry the merge if it has previously failed.
    Retry
  deriving (Eq, Show)

-- | For a PR to be approved a specific user must give a specific approval
--   command, i.e. either just "merge" or "merge and deploy".
data Approval = Approval
  { approver :: Username
  , approvalSource :: Maybe ReactableId
  , approvedFor :: ApprovedFor
  , approvalRetriedBy :: Maybe Username
  , approvalPriority :: Priority
  }
  deriving (Eq, Show, Generic)

data MergeWindow = OnFriday | DuringFeatureFreeze | AnyDay
  deriving (Eq, Show)

data Priority = Normal | High
  deriving (Eq, Show, Generic)

-- | A check is a key we check incoming build status contexts (in the case of
-- github) against.
newtype Check = Check Text
  deriving (Eq, Show, Ord)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

-- | We allow to specify multiple mandatory checks that should have succeeded
-- before being accepted, per project.
newtype MandatoryChecks = MandatoryChecks (Set Check)
  deriving (Eq, Show)
  deriving newtype (FromJSON, ToJSON, Monoid, Semigroup)

data PromotedPullRequest = PromotedPullRequest
  { promotedPRSha :: Sha
  , promotedPRTime :: UTCTime
  }
  deriving (Eq, Show, Generic)

data PullRequest = PullRequest
  { pullRequestId :: PullRequestId
  , sha :: Sha
  , branch :: Branch
  , baseBranch :: BaseBranch
  , title :: Text
  , author :: Username
  , approval :: Maybe Approval
  , integrationStatus :: IntegrationStatus
  , integrationAttempts :: [Sha]
  , needsFeedback :: Bool
  , pausedMessageSent :: Bool
  }
  deriving (Eq, Show, Generic)

data ProjectState = ProjectState
  { pullRequests :: IntMap PullRequest
  , -- All PRs that are approved are added to the merge train. Normal priority PRs are added to the
    -- end, High priority PRs behind existing High priority PRs. Approved PRs that have a wrong base
    -- branch (immediately) or wrong fixup commits (after rebase) are kicked from the train. PRs that
    -- fail to rebase or fail tests are kicked from the train once they are at the front of the train:
    -- before that they can potentially still succeed when the PR in front of them gets kicked from
    -- the train.
    mergeTrain :: [PullRequestId]
  , mandatoryChecks :: MandatoryChecks
  , recentlyPromoted :: [PromotedPullRequest]
  , paused :: Bool
  , lastSyncTime :: UTCTime
  }
  deriving (Eq, Show, Generic)

type Owner = Text

-- | Static information about a project, which does not change while the program
--   is running. Warning: Ordering owner before repository in the struct is
--   important as we're using that property for quick lookups via the usual Ord
--   instance.
data ProjectInfo = ProjectInfo
  { owner :: Owner
  , repository :: Text
  }
  deriving (Eq, Show, Ord)

-- | Get all repositories for a given owner, specialized to project info.
subMapByOwner :: Owner -> Map ProjectInfo a -> Map ProjectInfo a
subMapByOwner owner' perProjectInfo = do
  -- Use the antitone functions as the owner appears first in the ordering
  -- relation of project info. As a result of the ordering relation, we can
  -- neatly split the map such that we get the subset of repositories that
  -- match that specific owner using just two lookups.
  let
    subsetToCheck = Map.dropWhileAntitone (\info -> owner info < owner') perProjectInfo
    projectSubset = Map.takeWhileAntitone (\info -> owner info == owner') subsetToCheck
  projectSubset

-- Buildable instance for use with `format`,
-- mainly for nicer formatting in the logs.
instance Buildable ProjectInfo where
  build info =
    Text.fromText (owner info)
      <> Text.singleton '/'
      <> Text.fromText (repository info)

instance FromJSON PromotedPullRequest
instance FromJSON BuildStatus
instance FromJSON IntegrationStatus
instance FromJSON DeployEnvironment
instance FromJSON DeploySubprojects
instance FromJSON ApprovedFor
instance FromJSON Approval
instance FromJSON Priority

instance FromJSON ProjectState where
  parseJSON = Aeson.withObject "ProjectState" $ \o ->
    ProjectState
      <$> o .: "pullRequests"
      <*> o .: "mergeTrain"
      <*> o .: "mandatoryChecks"
      <*> o .: "recentlyPromoted"
      <*> o .: "paused"
      <*> o .:? "lastSyncTime" .!= lastSyncTime emptyProjectState

instance FromJSON PullRequest

instance ToJSON PromotedPullRequest where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON BuildStatus where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON IntegrationStatus where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON DeployEnvironment where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON DeploySubprojects where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON ApprovedFor where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON Approval where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON Priority where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON ProjectState where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON PullRequest where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- Reads and parses the state. Returns Nothing if parsing failed, but crashes if
-- the file could not be read.
loadProjectState :: FilePath -> IO (Either String ProjectState)
loadProjectState = fmap Aeson.eitherDecodeStrict' . readFile

saveProjectState :: FilePath -> ProjectState -> IO ()
saveProjectState fname state = do
  -- First write the file entirely, afterwards atomically move the new file over
  -- the old one. This way, the state file is never incomplete if the
  -- application is killed or crashes during a write.
  writeFile (fname ++ ".new") $ Aeson.encodePretty state
  renameFile (fname ++ ".new") fname

emptyProjectState :: ProjectState
emptyProjectState =
  ProjectState
    { pullRequests = IntMap.empty
    , mergeTrain = []
    , mandatoryChecks = mempty
    , recentlyPromoted = []
    , paused = False
    , lastSyncTime = UTCTime{utctDay = toEnum 0, utctDayTime = 0}
    }

-- Inserts a new pull request into the project, with approval set to Nothing,
-- build status to BuildNotStarted, and integration status to NotIntegrated.
insertPullRequest
  :: PullRequestId
  -> Branch
  -> BaseBranch
  -> Sha
  -> Text
  -> Username
  -> ProjectState
  -> ProjectState
insertPullRequest (PullRequestId n) prBranch bsBranch prSha prTitle prAuthor state =
  let pullRequest =
        PullRequest
          { pullRequestId = PullRequestId n
          , sha = prSha
          , branch = prBranch
          , baseBranch = bsBranch
          , title = prTitle
          , author = prAuthor
          , approval = Nothing
          , integrationStatus = NotIntegrated
          , integrationAttempts = []
          , needsFeedback = False
          , pausedMessageSent = False
          }
  in  state{pullRequests = IntMap.insert n pullRequest $ pullRequests state}

-- Removes the pull request detail from the project. This does not change the
-- integration candidate, which can be equal to the deleted pull request.
deletePullRequest :: PullRequestId -> ProjectState -> ProjectState
deletePullRequest pid@(PullRequestId n) state =
  removeFromTrain pid $ state{pullRequests = IntMap.delete n $ pullRequests state}

-- Returns whether the pull request is part of the set of open pull requests.
existsPullRequest :: PullRequestId -> ProjectState -> Bool
existsPullRequest (PullRequestId n) = IntMap.member n . pullRequests

lookupPullRequest :: PullRequestId -> ProjectState -> Maybe PullRequest
lookupPullRequest (PullRequestId n) = IntMap.lookup n . pullRequests

updatePullRequest :: PullRequestId -> (PullRequest -> PullRequest) -> ProjectState -> ProjectState
updatePullRequest (PullRequestId n) f state =
  state
    { pullRequests = IntMap.adjust f n $ pullRequests state
    }

updatePullRequests :: (PullRequest -> PullRequest) -> ProjectState -> ProjectState
updatePullRequests f state =
  state
    { pullRequests = IntMap.map f $ pullRequests state
    }

promotePullRequest :: PullRequest -> UTCTime -> ProjectState -> ProjectState
promotePullRequest pr currTime state =
  let
    prId = pullRequestId pr
    state' = setIntegrationStatus prId Promoted state
    state'' = case promotionSha pr of
      Just promotedSha -> state'{recentlyPromoted = PromotedPullRequest promotedSha currTime : recentlyPromoted state'}
      Nothing -> error "Can't add promoted pull request that is not being promoted."
  in
    unspeculateTrainHead $ deleteFromTrain prId state''

-- After promoting a PR, we have to make sure the new head of the train loses its speculative
-- failure status and promote it to a real failure. After this we send a message about the failure
-- and kick it from the merge train (in `Logic.proceedCandidate`)
unspeculateTrainHead :: ProjectState -> ProjectState
unspeculateTrainHead state = case mergeTrain state of
  -- No merge train, no problems
  [] -> state
  -- Update the new head from speculative failure or speculative conflict to real failure or conflict
  (pid : _) -> updatePullRequest pid (updatePRFailedIntegrated . updatePRConflicted) state
 where
  updatePRFailedIntegrated :: PullRequest -> PullRequest
  updatePRFailedIntegrated pr =
    if isFailedIntegrated (integrationStatus pr)
      then pr{needsFeedback = True}
      else pr

  updatePRConflicted :: PullRequest -> PullRequest
  updatePRConflicted pr = case integrationStatus pr of
    (Conflicted base reason)
      | base /= baseBranch pr ->
          pr
            { integrationStatus = Conflicted (baseBranch pr) reason
            , needsFeedback = True
            }
    _ -> pr

filterRecentlyPromoted :: (UTCTime -> Bool) -> ProjectState -> ProjectState
filterRecentlyPromoted p state = state{recentlyPromoted = filter (p . promotedPRTime) (recentlyPromoted state)}

addApproval :: PullRequestId -> Approval -> ProjectState -> ProjectState
addApproval pr approval = updatePullRequest pr (\pullRequest -> pullRequest{approval = Just approval, needsFeedback = True})

-- | Approves a pull request. The approval is placed at the back of the merge train, or behind the
-- priority PRs in the merge, depending on the Priority. If the pull request was already approved,
-- it is moved to the back for the respective train section.
approve :: PullRequestId -> Approval -> ProjectState -> ProjectState
approve pr approval state =
  let
    state' = addApproval pr approval state
    otherPRs = filter (/= pr) (mergeTrain state')
    isHighPriority p = case lookupPullRequest p state' of
      Just PullRequest{approval = Just a} -> approvalPriority a == High
      _ -> False
    highPrio = takeWhile isHighPriority otherPRs
    normalPrio = dropWhile isHighPriority otherPRs
  in
    case approvalPriority approval of
      Normal -> state'{mergeTrain = highPrio ++ normalPrio ++ [pr]}
      High ->
        let state'' = state'{mergeTrain = highPrio ++ [pr] ++ normalPrio}
        in  unintegrateAfter pr state''

-- | Given a pull request, mark all pull requests that follow from it
--   in the merge train as NotIntegrated
unintegrateAfter :: PullRequestId -> ProjectState -> ProjectState
unintegrateAfter pr0 state = foldl' unintegrate state (trainAfter pr0 state)
 where
  unintegrate :: ProjectState -> PullRequestId -> ProjectState
  unintegrate s pid = case lookupPullRequest pid state of
    Just pr
      | isIntegratedOrSpeculativelyConflicted pr ->
          setIntegrationStatus pid NotIntegrated s
    _ -> s

-- | Removes the pull request from the merge train, if it is in it. The
-- approval of the pull request is kept.
removeFromTrain :: PullRequestId -> ProjectState -> ProjectState
removeFromTrain pid state = deleteFromTrain pid $ unintegrateAfter pid state

deleteFromTrain :: PullRequestId -> ProjectState -> ProjectState
deleteFromTrain pid state = state{mergeTrain = filter (/= pid) (mergeTrain state)}

-- | The pull requests in the merge train, in order
trainPullRequests :: ProjectState -> [PullRequest]
trainPullRequests state = mapMaybe (\pr -> lookupPullRequest pr state) (mergeTrain state)

-- Sets the integration status for a pull request.
setIntegrationStatus :: PullRequestId -> IntegrationStatus -> ProjectState -> ProjectState
setIntegrationStatus pr newStatus = updatePullRequest pr changeIntegrationStatus
 where
  -- If there is a current integration candidate, remember it, so that we can
  -- ignore push webhook events for that commit (we probably pushed it
  -- ourselves, in any case it should not clear approval status).
  changeIntegrationStatus pullRequest = case integrationStatus pullRequest of
    Integrated oldSha _buildStatus ->
      pullRequest
        { integrationStatus = newStatus
        , integrationAttempts = oldSha : (integrationAttempts pullRequest)
        }
    _notIntegrated -> pullRequest{integrationStatus = newStatus}

setNeedsFeedback :: PullRequestId -> Bool -> ProjectState -> ProjectState
setNeedsFeedback pr value = updatePullRequest pr (\pullRequest -> pullRequest{needsFeedback = value})

classifyPullRequest :: PullRequest -> PullRequestStatus
classifyPullRequest pr = case approval pr of
  Nothing -> PrStatusAwaitingApproval
  Just _ -> case integrationStatus pr of
    NotIntegrated -> PrStatusApproved
    Outdated -> PrStatusOutdated
    IncorrectBaseBranch -> PrStatusIncorrectBaseBranch
    -- Fixups can be reported regardless of whether we are doing an speculative rebase
    Conflicted _ WrongFixups -> PrStatusWrongFixups
    Conflicted base _ | base /= baseBranch pr -> PrStatusSpeculativeConflict
    Conflicted _ EmptyRebase -> PrStatusEmptyRebase
    Conflicted _ _ -> PrStatusFailedConflict
    Integrated _ buildStatus -> case summarize buildStatus of
      BuildPending -> PrStatusBuildPending
      BuildStarted url -> PrStatusBuildStarted url
      BuildSucceeded -> PrStatusIntegrated
      BuildFailed url -> PrStatusFailedBuild url
    Promote _ _ -> PrStatusAwaitingPromotion
    PromoteAndTag{} -> PrStatusAwaitingPromotion
    Promoted -> PrStatusIntegrated

-- Returns the pull requests that satisfy the predicate, in the order defined in
-- `sortedPullRequests`
filterPullRequestsBy :: (PullRequest -> Bool) -> ProjectState -> [PullRequest]
filterPullRequestsBy p = filter p . sortedPullRequests

-- Return all PRs, in order of the merge train, the remainder in the PR number order
sortedPullRequests :: ProjectState -> [PullRequest]
sortedPullRequests state = trainPullRequests state ++ [pr | pr <- IntMap.elems (pullRequests state), pullRequestId pr `notElem` mergeTrain state]

-- Returns the number of pull requests that will be rebased and checked on CI
-- before the PR with the given id will be rebased, in case no other pull
-- requests get approved in the mean time (priority PRs can skip ahead).
getQueuePosition :: PullRequestId -> ProjectState -> Int
getQueuePosition pid = length . trainBefore pid

trainBefore :: PullRequestId -> ProjectState -> [PullRequestId]
trainBefore pr0 = takeWhile (/= pr0) . mergeTrain

-- As trainBefore, but also making sure we drop the pr itself
trainAfter :: PullRequestId -> ProjectState -> [PullRequestId]
trainAfter pr0 state = case dropWhile (/= pr0) $ mergeTrain state of
  (_ : prs) -> prs
  _ -> []

-- Returns whether a pull request is in the process of being integrated (pending
-- build results).
isInProgress :: PullRequest -> Bool
isInProgress pr = case approval pr of
  Nothing -> False
  Just _ -> case integrationStatus pr of
    NotIntegrated -> False
    Outdated -> False
    IncorrectBaseBranch -> False
    Conflicted _ _ -> False
    Integrated _ buildStatus -> case summarize buildStatus of
      BuildPending -> True
      BuildStarted _ -> True
      BuildSucceeded -> True
      BuildFailed _ -> False
    Promote _ _ -> True
    PromoteAndTag{} -> True
    Promoted -> False

-- Return whether the given commit is, or in this approval cycle ever was, an
-- integration candidate of this pull request.
wasIntegrationAttemptFor :: Sha -> PullRequest -> Bool
wasIntegrationAttemptFor commit pr = case integrationStatus pr of
  Integrated candidate _buildStatus -> commit `elem` (candidate : integrationAttempts pr)
  _ -> commit `elem` (integrationAttempts pr)

-- | Lists all PR ids that are speculative failures.
--
-- In other words, this lists all failed PRs
-- that come after the first non-failing PR
-- in approval order.
speculativelyFailedPullRequests :: ProjectState -> [PullRequest]
speculativelyFailedPullRequests state =
  filter (isFailedIntegrated . integrationStatus) $
    dropWhile
      (not . isUnfailedIntegrated . integrationStatus)
      (filter (isIntegrated . integrationStatus) $ trainPullRequests state)

-- | Returns the pull requests that have been approved, but for which integration
--   and building has not yet been attempted.
candidatePullRequests :: ProjectState -> [PullRequest]
candidatePullRequests state = filter ((\x -> x == NotIntegrated || x == Outdated) . integrationStatus) $ trainPullRequests state

-- | A string representation of a merge command, without the optional @ on
-- friday@ merge window suffix.
displayMergeCommand :: MergeCommand -> Text
displayMergeCommand (Approve Merge) = "merge"
displayMergeCommand (Approve MergeWithoutDeploy) = "merge without deploying"
displayMergeCommand (Approve (MergeAndDeploy subprojects (DeployEnvironment env))) =
  case subprojects of
    EntireProject -> format "merge and deploy to {}" [env]
    OnlySubprojects ss ->
      let subs = T.intercalate ", " ss
      in  format "merge and deploy {} to {}" (subs, env)
displayMergeCommand (Approve MergeAndTag) = "merge and tag"
displayMergeCommand Retry = "retry"

needsDeploy :: ApprovedFor -> Bool
needsDeploy Merge = False
needsDeploy MergeWithoutDeploy = False
needsDeploy MergeAndDeploy{} = True
needsDeploy MergeAndTag = False

needsTag :: ApprovedFor -> Bool
needsTag Merge = False
needsTag MergeWithoutDeploy = False
needsTag MergeAndDeploy{} = True
needsTag MergeAndTag = True

integrationSha :: PullRequest -> Maybe Sha
integrationSha PullRequest{integrationStatus = Integrated s _} = Just s
integrationSha _ = Nothing

integrationShas :: PullRequest -> [Sha]
integrationShas pr = maybeToList (integrationSha pr) ++ integrationAttempts pr

promotionSha :: PullRequest -> Maybe Sha
promotionSha pr = case integrationStatus pr of
  Promote _ newHead -> Just newHead
  PromoteAndTag _ newHead _ _ -> Just newHead
  _ -> Nothing

promotionTime :: PullRequest -> Maybe UTCTime
promotionTime pr = case integrationStatus pr of
  Promote time _ -> Just time
  PromoteAndTag time _ _ _ -> Just time
  _ -> Nothing

awaitingPromotion :: PullRequest -> Bool
awaitingPromotion pr = case integrationStatus pr of
  Promote _ _ -> True
  PromoteAndTag{} -> True
  _ -> False

isIntegrated :: IntegrationStatus -> Bool
isIntegrated (Integrated _ _) = True
isIntegrated (Promote _ _) = True
isIntegrated (PromoteAndTag{}) = True
isIntegrated _ = False

-- | Returns whether an 'IntegrationStatus' is integrated with a build failure:
--   @ Integrated _ (BuildFailed _) @
isFailedIntegrated :: IntegrationStatus -> Bool
isFailedIntegrated (Integrated _ (summarize -> BuildFailed _)) = True
isFailedIntegrated _ = False

-- | Returns whether an 'IntegrationStatus' is integrated
--   without a build failure, i.e. build pending, started or succeeded.
isUnfailedIntegrated :: IntegrationStatus -> Bool
isUnfailedIntegrated (Integrated _ buildStatus) = case summarize buildStatus of
  BuildPending -> True
  (BuildStarted _) -> True
  BuildSucceeded -> True
  (BuildFailed _) -> False
isUnfailedIntegrated (Promote _ _) = True
isUnfailedIntegrated (PromoteAndTag{}) = True
isUnfailedIntegrated _ = False

-- | Returns whether a 'PullRequest' is integrated or conflicted speculatively.
isIntegratedOrSpeculativelyConflicted :: PullRequest -> Bool
isIntegratedOrSpeculativelyConflicted pr =
  case integrationStatus pr of
    (Integrated _ _) -> True
    (Promote _ _) -> True
    (PromoteAndTag{}) -> True
    (Conflicted base _) | base /= baseBranch pr -> True
    _ -> False

summarize :: OutstandingChecks -> BuildStatus
summarize (AnyCheck status) = status
summarize (SpecificChecks statusChecks) =
  let
    go :: [BuildStatus] -> BuildStatus
    go checks =
      if
        | all (== BuildSucceeded) checks -> BuildSucceeded
        | Just failure <- findFirst checks isBuildFailed -> failure
        | Just started <- findFirst checks isBuildStarted -> started
        | otherwise -> BuildPending
    findFirst checks isConstr = asum (map isConstr checks)
  in
    go $ map snd $ Map.toList statusChecks

-- | Does the first build status supersedes the second?
--
-- * The same status does not supersede itself
--
-- * Statuses 'BuildSuceeded' and 'BuildFailed' are not
--   superseded by other statuses
--
-- * Statuses 'BuildFailed' does supersede 'BuildSucceeded'. In the case of
--   multiple mandatory checks, we want failures to always be reported.
--
-- * We ignore changes of just the URL.
--
-- This is used in 'handleBuildStatusChanged`.
--
-- This is needed because there may be two concurrent builds pointing to the
-- same commit hash depending on the CI you are using with GitHub.
-- This guarantees that 'BuildSucceeded' or 'BuildFailed' statuses
-- cannot be overridden by later builds of a branch pointing to the same hash.
supersedes :: BuildStatus -> BuildStatus -> Bool
supersedes newStatus oldStatus | sameStatus newStatus oldStatus = False
supersedes (BuildFailed _) BuildSucceeded = True
supersedes _ oldStatus = not (isFinalStatus oldStatus)

isFinalStatus :: BuildStatus -> Bool
isFinalStatus (BuildFailed _) = True
isFinalStatus BuildSucceeded = True
isFinalStatus _ = False

-- | Compares if two build statuses are the same
--   while ignoring any URL arguments
sameStatus :: BuildStatus -> BuildStatus -> Bool
sameStatus BuildStarted{} BuildStarted{} = True
sameStatus BuildFailed{} BuildFailed{} = True
sameStatus status1 status2 = status1 == status2

-- | Total function for checking and returning whether the build status
-- corresponds to check failure
isBuildFailed :: BuildStatus -> Maybe BuildStatus
isBuildFailed s@(BuildFailed _) = Just s
isBuildFailed _ = Nothing

-- | Total function for checking and returning whether the build status
-- corresponds to a start of a check
isBuildStarted :: BuildStatus -> Maybe BuildStatus
isBuildStarted s@(BuildStarted _) = Just s
isBuildStarted _ = Nothing

pause :: ProjectState -> ProjectState
pause state = state{paused = True}

resume :: ProjectState -> ProjectState
resume state = updatePullRequests (\pr -> pr{pausedMessageSent = False}) state{paused = False}
