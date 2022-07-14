-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project
(
  Approval (..),
  ApprovedFor (..),
  BuildStatus (..),
  IntegrationStatus (..),
  ProjectInfo (..),
  ProjectState (..),
  PullRequest (..),
  PullRequestId (..),
  PullRequestStatus (..),
  integrationCandidate,
  Owner,
  approvedPullRequests,
  integratedPullRequests,
  candidatePullRequests,
  classifyPullRequest,
  classifyPullRequests,
  deletePullRequest,
  emptyProjectState,
  existsPullRequest,
  getIntegrationCandidate,
  getQueuePosition,
  insertPullRequest,
  loadProjectState,
  lookupPullRequest,
  saveProjectState,
  alwaysAddMergeCommit,
  needsDeploy,
  needsTag,
  displayApproval,
  setApproval,
  newApprovalOrder,
  setIntegrationStatus,
  setNeedsFeedback,
  updatePullRequest,
  updatePullRequests,
  getOwners,
  wasIntegrationAttemptFor,
  MergeWindow(..))
where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (readFile)
import Data.ByteString.Lazy (writeFile)
import Data.IntMap.Strict (IntMap)
import Data.List (intersect, nub, sortBy)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import GHC.Generics
import Git (Branch (..), BaseBranch (..), Sha (..), GitIntegrationFailure (..))
import Prelude hiding (readFile, writeFile)
import System.Directory (renameFile)

import Data.Text.Buildable (Buildable (build))
import Data.Text.Lazy.Builder as Text

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.IntMap.Strict as IntMap

import Types (PullRequestId (..), Username)

data BuildStatus
  = BuildPending
  | BuildSucceeded
  | BuildFailed (Maybe Text)
  deriving (Eq, Show, Generic)

-- When attempting to integrated changes, there can be five states:
--
-- * no attempt has been made to integrate;
--
-- * integration (e.g. merge or rebase) was successful
--   and the new commit has the given sha;
--
-- * the PR has been promoted to be the new master;
--
-- * and an attempt to integrate was made, but it wasn't successful.
data IntegrationStatus
  = NotIntegrated
  | Integrated Sha BuildStatus
  | Promoted
  | Conflicted BaseBranch GitIntegrationFailure
  | IncorrectBaseBranch
  deriving (Eq, Show, Generic)

data PullRequestStatus
  = PrStatusAwaitingApproval          -- New, awaiting review.
  | PrStatusApproved                  -- Approved, but not yet integrated or built.
  | PrStatusBuildPending              -- Integrated, and build pending or in progress.
  | PrStatusIntegrated                -- Integrated, build passed, merged into target branch.
  | PrStatusIncorrectBaseBranch       -- ^ Integration branch not being valid.
  | PrStatusWrongFixups               -- Failed to integrate due to the presence of orphan fixup commits.
  | PrStatusEmptyRebase               -- Rebase was empty (changes already in the target branch?)
  | PrStatusFailedConflict            -- Failed to integrate due to merge conflict.
  | PrStatusFailedBuild (Maybe Text)  -- Integrated, but the build failed. Field should contain the URL to a page explaining the build failure.
  deriving (Eq)

-- A PR can be approved to be merged with "<prefix> merge", or it can be
-- approved to be merged and also deployed with "<prefix> merge and deploy".
-- This enumeration distinguishes these cases.
data ApprovedFor
  = Merge
  | MergeAndDeploy
  | MergeAndTag
  deriving (Eq, Show, Generic)

-- For a PR to be approved a specific user must give a specific approval
-- command, i.e. either just "merge" or "merge and deploy".
data Approval = Approval
  { approver    :: Username
  , approvedFor :: ApprovedFor
  , approvalOrder :: Int
  }
  deriving (Eq, Show, Generic)

data MergeWindow = OnFriday | NotFriday

data PullRequest = PullRequest
  { sha                 :: Sha
  , branch              :: Branch
  , baseBranch          :: BaseBranch
  , title               :: Text
  , author              :: Username
  , approval            :: Maybe Approval
  , integrationStatus   :: IntegrationStatus
  , integrationAttempts :: [Sha]
  , needsFeedback       :: Bool
  }
  deriving (Eq, Show, Generic)

data ProjectState = ProjectState
  { pullRequests              :: IntMap PullRequest
  , pullRequestApprovalIndex  :: Int
  }
  deriving (Eq, Show, Generic)

type Owner = Text

-- Static information about a project, which does not change while the program
-- is running.
data ProjectInfo = ProjectInfo
  {
    owner      :: Owner,
    repository :: Text
  }
  deriving (Eq, Show)

-- Buildable instance for use with `format`,
-- mainly for nicer formatting in the logs.
instance Buildable ProjectInfo where
  build info
    =  Text.fromText (owner info)
    <> Text.singleton '/'
    <> Text.fromText (repository info)

instance FromJSON BuildStatus
instance FromJSON IntegrationStatus
instance FromJSON ApprovedFor
instance FromJSON Approval
instance FromJSON ProjectState
instance FromJSON PullRequest

instance ToJSON BuildStatus where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON IntegrationStatus where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON ApprovedFor where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON Approval where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
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
emptyProjectState = ProjectState {
  pullRequests         = IntMap.empty,
  pullRequestApprovalIndex = 0
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
  let
    pullRequest = PullRequest {
        sha                 = prSha,
        branch              = prBranch,
        baseBranch          = bsBranch,
        title               = prTitle,
        author              = prAuthor,
        approval            = Nothing,
        integrationStatus   = NotIntegrated,
        integrationAttempts = [],
        needsFeedback       = False
      }
  in state { pullRequests = IntMap.insert n pullRequest $ pullRequests state }

-- Removes the pull request detail from the project. This does not change the
-- integration candidate, which can be equal to the deleted pull request.
deletePullRequest :: PullRequestId -> ProjectState -> ProjectState
deletePullRequest (PullRequestId n) state = state {
  pullRequests = IntMap.delete n $ pullRequests state
}

-- Returns whether the pull request is part of the set of open pull requests.
existsPullRequest :: PullRequestId -> ProjectState -> Bool
existsPullRequest (PullRequestId n) = IntMap.member n . pullRequests

lookupPullRequest :: PullRequestId -> ProjectState -> Maybe PullRequest
lookupPullRequest (PullRequestId n) = IntMap.lookup n . pullRequests

updatePullRequest :: PullRequestId -> (PullRequest -> PullRequest) -> ProjectState -> ProjectState
updatePullRequest (PullRequestId n) f state = state {
  pullRequests = IntMap.adjust f n $ pullRequests state
}

updatePullRequests :: (PullRequest -> PullRequest) -> ProjectState -> ProjectState
updatePullRequests f state = state {
  pullRequests = IntMap.map f $ pullRequests state
}

-- Marks the pull request as approved by somebody or nobody.
setApproval :: PullRequestId -> Maybe Approval -> ProjectState -> ProjectState
setApproval pr newApproval = updatePullRequest pr changeApproval
  where changeApproval pullRequest = pullRequest { approval = newApproval }

newApprovalOrder :: ProjectState -> (Int, ProjectState)
newApprovalOrder state =
  let index = pullRequestApprovalIndex state
  in (index, state{ pullRequestApprovalIndex = index + 1})

-- Sets the integration status for a pull request.
setIntegrationStatus :: PullRequestId -> IntegrationStatus -> ProjectState -> ProjectState
setIntegrationStatus pr newStatus = updatePullRequest pr changeIntegrationStatus
  where
    -- If there is a current integration candidate, remember it, so that we can
    -- ignore push webhook events for that commit (we probably pushed it
    -- ourselves, in any case it should not clear approval status).
    changeIntegrationStatus pullRequest = case integrationStatus pullRequest of
      Integrated oldSha _buildStatus -> pullRequest
        { integrationStatus = newStatus
        , integrationAttempts = oldSha : (integrationAttempts pullRequest)
        }
      _notIntegrated -> pullRequest { integrationStatus = newStatus }

-- Here for backwards compatibility (TODO: remove?)
integrationCandidate :: ProjectState -> Maybe PullRequestId
integrationCandidate = fmap fst . getIntegrationCandidate

-- Gets the first integration candidate
getIntegrationCandidate :: ProjectState -> Maybe (PullRequestId, PullRequest)
getIntegrationCandidate = listToMaybe . getIntegrationCandidates

-- Same as 'integratedPullRequests' but paired with the underlying object.
getIntegrationCandidates :: ProjectState -> [(PullRequestId, PullRequest)]
getIntegrationCandidates state =
  [ (pullRequestId, candidate)
  | pullRequestId <- integratedPullRequests state
  , Just candidate <- [lookupPullRequest pullRequestId state]
  ]

setNeedsFeedback :: PullRequestId -> Bool -> ProjectState -> ProjectState
setNeedsFeedback pr value = updatePullRequest pr (\pullRequest -> pullRequest { needsFeedback = value })

classifyPullRequest :: PullRequest -> PullRequestStatus
classifyPullRequest pr = case approval pr of
  Nothing -> PrStatusAwaitingApproval
  Just _  -> case integrationStatus pr of
    NotIntegrated -> PrStatusApproved
    IncorrectBaseBranch -> PrStatusIncorrectBaseBranch
    Conflicted _ WrongFixups -> PrStatusWrongFixups
    Conflicted _ EmptyRebase -> PrStatusEmptyRebase
    Conflicted _ _  -> PrStatusFailedConflict
    Integrated _ buildStatus -> case buildStatus of
      BuildPending    -> PrStatusBuildPending
      BuildSucceeded  -> PrStatusIntegrated
      BuildFailed url -> PrStatusFailedBuild url
    Promoted -> PrStatusIntegrated -- TODO: state-of-its-own?

-- Classify every pull request into one status. Orders pull requests by id in
-- ascending order.
classifyPullRequests :: ProjectState -> [(PullRequestId, PullRequest, PullRequestStatus)]
classifyPullRequests state = IntMap.foldMapWithKey aux (pullRequests state)
  where
    aux i pr = [(PullRequestId i, pr, classifyPullRequest pr)]

-- Returns the ids of the pull requests that satisfy the predicate, in ascending
-- order. The ids are sorted by the approval order, with not yet approved PRs
-- at the end of the list.
filterPullRequestsBy :: (PullRequest -> Bool) -> ProjectState -> [PullRequestId]
filterPullRequestsBy p =
  fmap PullRequestId
  . map fst
  . sortBy comp
  . IntMap.toList
  . IntMap.filter p
  . pullRequests
  where
    -- Compare the approval orders, prefer a Just over a Nothing
    comp x y = comp' (approvalOrder <$> approval (snd x)) (approvalOrder <$> approval (snd y))
    comp' Nothing Nothing = EQ
    comp' (Just _) Nothing = LT
    comp' Nothing (Just _) = GT
    comp' (Just n) (Just m) = compare n m

-- Returns the pull requests that have been approved, in order of ascending id.
approvedPullRequests :: ProjectState -> [PullRequestId]
approvedPullRequests = filterPullRequestsBy $ isJust . approval

-- Returns the number of pull requests that will be rebased and checked on CI
-- before the PR with the given id will be rebased, in case no other pull
-- requests get approved in the mean time (PRs with a lower id may skip ahead).
getQueuePosition :: PullRequestId -> ProjectState -> Int
getQueuePosition prIndex state =
  let
    approvalNumber = maybe maxBound approvalOrder (lookupPullRequest prIndex state >>= approval)
    isEarlier pr = isQueued pr && maybe maxBound approvalOrder (approval pr) < approvalNumber
    queue = filterPullRequestsBy isEarlier state
    inProgress = filterPullRequestsBy isInProgress state
  in
    length (inProgress ++ queue)

-- Returns whether a pull request is queued for merging, but not already in
-- progress (pending build results).
isQueued :: PullRequest -> Bool
isQueued pr = case approval pr of
  Nothing -> False
  Just _  -> case integrationStatus pr of
    NotIntegrated  -> True
    IncorrectBaseBranch -> False
    Conflicted _ _ -> False
    Integrated _ _ -> False
    Promoted -> False

-- Returns whether a pull request is in the process of being integrated (pending
-- build results).
isInProgress :: PullRequest -> Bool
isInProgress pr = case approval pr of
  Nothing -> False
  Just _  -> case integrationStatus pr of
    NotIntegrated -> False
    IncorrectBaseBranch -> False
    Conflicted _ _ -> False
    Integrated _ buildStatus -> case buildStatus of
      BuildPending   -> True
      BuildSucceeded -> False
      BuildFailed _  -> False
    Promoted -> False

-- Return whether the given commit is, or in this approval cycle ever was, an
-- integration candidate of this pull request.
wasIntegrationAttemptFor :: Sha -> PullRequest -> Bool
wasIntegrationAttemptFor commit pr = case integrationStatus pr of
  Integrated candidate _buildStatus -> commit `elem` (candidate : integrationAttempts pr)
  _                                 -> commit `elem` (integrationAttempts pr)

integratedPullRequests :: ProjectState -> [PullRequestId]
integratedPullRequests = filterPullRequestsBy $ isIntegrated . integrationStatus
  where
  isIntegrated (Integrated _ _) = True
  isIntegrated _                = False

-- Returns the pull requests that have not been integrated yet, in order of
-- ascending id.
unintegratedPullRequests :: ProjectState -> [PullRequestId]
unintegratedPullRequests = filterPullRequestsBy $ (== NotIntegrated) . integrationStatus

-- Returns the pull requests that have been approved, but for which integration
-- and building has not yet been attempted.
candidatePullRequests :: ProjectState -> [PullRequestId]
candidatePullRequests state =
  let
    approved     = approvedPullRequests state
    unintegrated = unintegratedPullRequests state
  in
    approved `intersect` unintegrated

getOwners :: [ProjectInfo] -> [Owner]
getOwners = nub . map owner

displayApproval :: ApprovedFor -> Text
displayApproval Merge          = "merge"
displayApproval MergeAndDeploy = "merge and deploy"
displayApproval MergeAndTag    = "merge and tag"

alwaysAddMergeCommit :: ApprovedFor -> Bool
alwaysAddMergeCommit Merge          = False
alwaysAddMergeCommit MergeAndDeploy = True
alwaysAddMergeCommit MergeAndTag    = False

needsDeploy :: ApprovedFor -> Bool
needsDeploy Merge          = False
needsDeploy MergeAndDeploy = True
needsDeploy MergeAndTag    = False

needsTag :: ApprovedFor -> Bool
needsTag Merge          = False
needsTag MergeAndDeploy = True
needsTag MergeAndTag    = True
