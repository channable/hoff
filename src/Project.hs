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
  BuildStatus (..),
  IntegrationStatus (..),
  ProjectInfo (..),
  ProjectState (..),
  PullRequest (..),
  PullRequestId (..),
  PullRequestStatus (..),
  Owner,
  approvedPullRequests,
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
  setApproval,
  setIntegrationCandidate,
  setIntegrationStatus,
  setNeedsFeedback,
  updatePullRequest,
  getOwners,
  wasIntegrationAttemptFor,
)
where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (readFile)
import Data.ByteString.Lazy (writeFile)
import Data.IntMap.Strict (IntMap)
import Data.List (intersect, nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Generics
import Git (Branch (..), Sha (..))
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
  | BuildFailed
  deriving (Eq, Show, Generic)

-- When attempting to integrated changes, there can be three states: no attempt
-- has been made to integrate; integration (e.g. merge or rebase) was successful
-- and the new commit has the given sha; and an attempt to integrate was made,
-- but it resulted in merge conflicts.
data IntegrationStatus
  = NotIntegrated
  | Integrated Sha BuildStatus
  | Conflicted Branch
  deriving (Eq, Show, Generic)

data PullRequestStatus
  = PrStatusAwaitingApproval -- New, awaiting review.
  | PrStatusApproved         -- Approved, but not yet integrated or built.
  | PrStatusBuildPending     -- Integrated, and build pending or in progress.
  | PrStatusIntegrated       -- Integrated, build passed, merged into target branch.
  | PrStatusFailedConflict   -- Failed to integrate due to merge conflict.
  | PrStatusFailedBuild      -- Integrated, but the build failed.
  deriving (Eq)

data PullRequest = PullRequest
  { sha                 :: Sha
  , branch              :: Branch
  , base                :: Branch
  , title               :: Text
  , author              :: Username
  , approvedBy          :: Maybe Username
  , integrationStatus   :: IntegrationStatus
  , integrationAttempts :: [Sha]
  , needsFeedback       :: Bool
  }
  deriving (Eq, Show, Generic)

data ProjectState = ProjectState
  {
    pullRequests         :: IntMap PullRequest,
    integrationCandidate :: Maybe PullRequestId
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
instance FromJSON ProjectState
instance FromJSON PullRequest

instance ToJSON BuildStatus where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON IntegrationStatus where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON ProjectState where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON PullRequest where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- Reads and parses the state. Returns Nothing if parsing failed, but crashes if
-- the file could not be read.
loadProjectState :: FilePath -> IO (Maybe ProjectState)
loadProjectState = fmap Aeson.decodeStrict' . readFile

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
  integrationCandidate = Nothing
}

-- Inserts a new pull request into the project, with approval set to Nothing,
-- build status to BuildNotStarted, and integration status to NotIntegrated.
insertPullRequest
  :: PullRequestId
  -> Branch
  -> Sha
  -> Branch
  -> Text
  -> Username
  -> ProjectState
  -> ProjectState
insertPullRequest (PullRequestId n) prBranch prSha prBase prTitle prAuthor state =
  let pullRequest = PullRequest {
        sha                 = prSha,
        branch              = prBranch,
        base                = prBase,
        title               = prTitle,
        author              = prAuthor,
        approvedBy          = Nothing,
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

-- Marks the pull request as approved by somebody or nobody.
setApproval :: PullRequestId -> Maybe Username -> ProjectState -> ProjectState
setApproval pr newApprovedBy = updatePullRequest pr changeApproval
  where changeApproval pullRequest = pullRequest { approvedBy = newApprovedBy }

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

getIntegrationCandidate :: ProjectState -> Maybe (PullRequestId, PullRequest)
getIntegrationCandidate state = do
  pullRequestId <- integrationCandidate state
  candidate     <- lookupPullRequest pullRequestId state
  return (pullRequestId, candidate)

setIntegrationCandidate :: Maybe PullRequestId -> ProjectState -> ProjectState
setIntegrationCandidate pr state = state {
  integrationCandidate = pr
}

setNeedsFeedback :: PullRequestId -> Bool -> ProjectState -> ProjectState
setNeedsFeedback pr value state =
  updatePullRequest pr (\pullRequest -> pullRequest { needsFeedback = value }) state

classifyPullRequest :: PullRequest -> PullRequestStatus
classifyPullRequest pr = case approvedBy pr of
  Nothing -> PrStatusAwaitingApproval
  Just _  -> case integrationStatus pr of
    NotIntegrated -> PrStatusApproved
    Conflicted _  -> PrStatusFailedConflict
    Integrated _ buildStatus -> case buildStatus of
      BuildPending   -> PrStatusBuildPending
      BuildSucceeded -> PrStatusIntegrated
      BuildFailed    -> PrStatusFailedBuild

-- Classify every pull request into one status. Orders pull requests by id in
-- ascending order.
classifyPullRequests :: ProjectState -> [(PullRequestId, PullRequest, PullRequestStatus)]
classifyPullRequests state = IntMap.foldMapWithKey aux (pullRequests state)
  where
    aux i pr = [(PullRequestId i, pr, classifyPullRequest pr)]

-- Returns the ids of the pull requests that satisfy the predicate, in ascending
-- order.
filterPullRequestsBy :: (PullRequest -> Bool) -> ProjectState -> [PullRequestId]
filterPullRequestsBy p =
  fmap PullRequestId . IntMap.keys . IntMap.filter p . pullRequests

-- Returns the pull requests that have been approved, in order of ascending id.
approvedPullRequests :: ProjectState -> [PullRequestId]
approvedPullRequests = filterPullRequestsBy $ isJust . approvedBy

-- Returns the number of pull requests that will be rebased and checked on CI
-- before the PR with the given id will be rebased, in case no other pull
-- requests get approved in the mean time (PRs with a lower id may skip ahead).
getQueuePosition :: PullRequestId -> ProjectState -> Int
getQueuePosition pr state =
  let
    queue = filter (< pr) $ filterPullRequestsBy isQueued state
    inProgress = filterPullRequestsBy isInProgress state
  in
    length (inProgress ++ queue)

-- Returns whether a pull request is queued for merging, but not already in
-- progress (pending build results).
isQueued :: PullRequest -> Bool
isQueued pr = case approvedBy pr of
  Nothing -> False
  Just _  -> case integrationStatus pr of
    NotIntegrated  -> True
    Conflicted _   -> False
    Integrated _ _ -> False

-- Returns whether a pull request is in the process of being integrated (pending
-- build results).
isInProgress :: PullRequest -> Bool
isInProgress pr = case approvedBy pr of
  Nothing -> False
  Just _  -> case integrationStatus pr of
    NotIntegrated -> False
    Conflicted _  -> False
    Integrated _ buildStatus -> case buildStatus of
      BuildPending   -> True
      BuildSucceeded -> False
      BuildFailed    -> False

-- Return whether the given commit is, or in this approval cycle ever was, an
-- integration candidate of this pull request.
wasIntegrationAttemptFor :: Sha -> PullRequest -> Bool
wasIntegrationAttemptFor commit pr = case integrationStatus pr of
  Integrated candidate _buildStatus -> commit `elem` (candidate : integrationAttempts pr)
  _                                 -> commit `elem` (integrationAttempts pr)

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
