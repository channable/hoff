-- Hoff -- A gatekeeper for your commits
-- Copyright 2019 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- This module defines high-level Github API operations, plus an interpreter to
-- run those operations against the real API.
module GithubApi
(
  GithubOperation (..),
  PullRequest (..),
  ReactionContent(..),
  getOpenPullRequests,
  getPullRequest,
  hasPushAccess,
  leaveComment,
  addReaction,
  runGithub,
  runGithubReadOnly,
)
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN, logInfoN, logWarnN, logErrorN)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send, interpose)
import Data.IntSet (IntSet)
import Data.Text (Text)
import GitHub.Data.Reactions (ReactionContent(..))

import qualified Data.IntSet as IntSet
import qualified Data.Vector as Vector
import qualified GitHub.Data.Id as Github3
import qualified GitHub.Data.Name as Github3
import qualified GitHub.Data.Options as Github3
import qualified GitHub.Endpoints.Issues.Comments as Github3
import qualified GitHub.Endpoints.PullRequests as Github3
import qualified GitHub.Endpoints.Reactions as Github3
import qualified GitHub.Endpoints.Repos.Collaborators as Github3
import qualified GitHub.Request as Github3
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Http

import Format (format)
import Git (BaseBranch (..), Branch (..), Sha (..))
import MonadLoggerEffect (MonadLoggerEffect)
import Project (ProjectInfo)
import Types (PullRequestId (..), Username (..), CommentId (..), ReactableId (..))

import qualified Project

-- A stripped-down version of the `Github3.PullRequest` type, with only the
-- fields we need.
data PullRequest = PullRequest
  { sha    :: Sha
  , branch :: Branch
  , baseBranch :: BaseBranch
  , title  :: Text
  , author :: Username
  }

data GithubOperation :: Effect where
  LeaveComment :: PullRequestId -> Text -> GithubOperation m ()
  AddReaction :: ReactableId -> ReactionContent -> GithubOperation m ()
  HasPushAccess :: Username -> GithubOperation m Bool
  GetPullRequest :: PullRequestId -> GithubOperation m (Maybe PullRequest)
  GetOpenPullRequests :: GithubOperation m (Maybe IntSet)

type instance DispatchOf GithubOperation = 'Dynamic

leaveComment :: GithubOperation :> es => PullRequestId -> Text -> Eff es ()
leaveComment pr remoteBranch = send $ LeaveComment pr remoteBranch

addReaction :: GithubOperation :> es => ReactableId -> ReactionContent -> Eff es ()
addReaction id' reaction = send $ AddReaction id' reaction

hasPushAccess :: GithubOperation :> es => Username -> Eff es Bool
hasPushAccess username = send $ HasPushAccess username

getPullRequest :: GithubOperation :> es => PullRequestId -> Eff es (Maybe PullRequest)
getPullRequest pr = send $ GetPullRequest pr

getOpenPullRequests :: GithubOperation :> es => Eff es (Maybe IntSet)
getOpenPullRequests = send GetOpenPullRequests

isPermissionToPush :: Github3.CollaboratorPermission -> Bool
isPermissionToPush perm = case perm of
  Github3.CollaboratorPermissionAdmin -> True
  Github3.CollaboratorPermissionWrite -> True
  Github3.CollaboratorPermissionRead -> False
  Github3.CollaboratorPermissionNone -> False

pattern StatusCodeException :: Http.Response() -> Github3.Error
pattern StatusCodeException response <-
  Github3.HTTPError (
    Http.HttpExceptionRequest _request (Http.StatusCodeException response _body)
  )

is404NotFound :: Github3.Error -> Bool
is404NotFound err = case err of
  StatusCodeException response -> Http.responseStatus response == Http.notFound404
  _ -> False

runGithub
  :: (IOE :> es, MonadLoggerEffect :> es)
  => Github3.Auth
  -> ProjectInfo
  -> Eff (GithubOperation : es) a
  -> Eff es a
runGithub auth projectInfo =
  interpret $ \_ -> \case
    LeaveComment (PullRequestId pr) body -> do
      result <- liftIO $ Github3.github auth $ Github3.createCommentR
        (Github3.N $ Project.owner projectInfo)
        (Github3.N $ Project.repository projectInfo)
        (Github3.IssueNumber pr)
        body
      case result of
        Left err -> logWarnN $ format "Failed to comment: {}" [show err]
        Right _ -> logInfoN $ format "Posted comment on {}#{}: {}"
                                     (Project.repository projectInfo, pr, body)

    AddReaction reactableId reaction -> do
      let
        createReactionR project owner =
          case reactableId of
            OnIssueComment (CommentId commentId) -> Github3.createCommentReactionR project owner (Github3.Id commentId)
            OnPullRequest (PullRequestId prId) -> Github3.createIssueReactionR project owner (Github3.Id prId)

      result <- liftIO $ Github3.github auth $ createReactionR
        (Github3.N $ Project.owner projectInfo)
        (Github3.N $ Project.repository projectInfo)
        reaction

      case result of
        Left err -> logWarnN $ format "Failed to add reaction: {}" [show err]
        Right _ ->
          logInfoN $
            format
              "Added reaction in {} on {}: {}"
              (Project.repository projectInfo, reactableId, show reaction)

    HasPushAccess (Username username) -> do
      result <- liftIO $ Github3.github auth $ Github3.collaboratorPermissionOnR
        (Github3.N $ Project.owner projectInfo)
        (Github3.N $ Project.repository projectInfo)
        (Github3.N username)

      case result of
        Left err -> do
          logErrorN $ format "Failed to retrive collaborator status: {}" [show err]
          -- To err on the safe side, if the API call fails, we pretend nobody
          -- has push access.
          pure False

        Right (Github3.CollaboratorWithPermission _user perm) -> do
          logDebugN $ format "User {} has permission {} on {}." (username, show perm, projectInfo)
          pure $ isPermissionToPush perm

    GetPullRequest (PullRequestId pr) -> do
      logDebugN $ format "Getting pull request {} in {}." (pr, projectInfo)
      result <- liftIO $ Github3.github auth $ Github3.pullRequestR
        (Github3.N $ Project.owner projectInfo)
        (Github3.N $ Project.repository projectInfo)
        (Github3.IssueNumber pr)
      case result of
        Left err | is404NotFound err -> do
          logWarnN $ format "Pull request {} does not exist in {}." (pr, projectInfo)
          pure Nothing
        Left err -> do
          logWarnN $ format "Failed to retrieve pull request {} in {}: {}" (pr, projectInfo, show err)
          pure Nothing
        Right details -> pure $ Just $ PullRequest
          { sha    = Sha $ Github3.pullRequestCommitSha $ Github3.pullRequestHead details
          , branch = Branch $ Github3.pullRequestCommitRef $ Github3.pullRequestHead details
          , baseBranch = BaseBranch $ Github3.pullRequestCommitRef $ Github3.pullRequestBase details
          , title  = Github3.pullRequestTitle details
          , author = Username $ Github3.untagName $ Github3.simpleUserLogin $ Github3.pullRequestUser details
          }

    GetOpenPullRequests -> do
      logDebugN $ format "Getting open pull request in {}." [projectInfo]
      result <- liftIO $ Github3.github auth $ Github3.pullRequestsForR
        (Github3.N $ Project.owner projectInfo)
        (Github3.N $ Project.repository projectInfo)
        Github3.stateOpen
        Github3.FetchAll
      case result of
        Left err -> do
          logWarnN $ format "Failed to retrieve pull requests in {}: {}" (projectInfo, show err)
          pure Nothing
        Right prs -> do
          logDebugN $ format "Got {} open pull requests in {}." (Vector.length prs, projectInfo)
          pure $ Just
            -- Note: we want to extract the *issue number*, not the *id*,
            -- which is a different integer part of the payload.
            $ foldMap (IntSet.singleton . Github3.unIssueNumber . Github3.simplePullRequestNumber)
            $ prs

-- Like runGithub, but does not execute operations that have side effects, in
-- the sense of being observable by Github users. We will still make requests
-- against the read-only endpoints of the API. This is useful for local testing.
runGithubReadOnly
  :: (IOE :> es, MonadLoggerEffect :> es)
  => Github3.Auth
  -> ProjectInfo
  -> Eff (GithubOperation : es) a
  -> Eff es a
runGithubReadOnly auth projectInfo = runGithub auth projectInfo . augmentedGithubOperation
  where
    augmentedGithubOperation = interpose $ \_ operation -> case operation of
      -- These operations are read-only, we can run them for real.
      HasPushAccess username -> send $ HasPushAccess username
      GetPullRequest pullRequestId -> send $ GetPullRequest pullRequestId
      GetOpenPullRequests -> send GetOpenPullRequests

      -- These operations have side effects, we fake them.
      LeaveComment pr body ->
        logInfoN $ format "Would have posted comment on {}: {}" (show pr, body)
      AddReaction reactableId reaction ->
        logInfoN $ format "Would have added reaction on {}: {}" (reactableId, show reaction)
