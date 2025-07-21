-- Hoff -- A gatekeeper for your commits
-- Copyright 2020 The Hoff authors
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (
  Body (..),
  PullRequestId (..),
  CommentId (..),
  ReactableId (..),
  Username (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Buildable (Buildable (..))
import GHC.Generics (Generic)

import Data.Aeson qualified as Aeson

-- The name of a user on GitHub.
newtype Username = Username Text deriving (Eq, Show, Generic, IsString, Buildable)

-- A pull request is identified by its number.
newtype PullRequestId = PullRequestId Int deriving (Eq, Ord, Show, Generic)

-- The body of a pull request
newtype Body = Body Text deriving (Eq, Show, Generic, IsString, Buildable)

-- The numeric ID of an issue comment. (In GitHub's model, a PR is a special kind of issue.)
newtype CommentId = CommentId Int
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Body
instance FromJSON PullRequestId
instance FromJSON Username
instance FromJSON CommentId

instance ToJSON Body where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON PullRequestId where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON Username where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON CommentId where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- The numeric ID of something on GitHub we can react to.
data ReactableId
  = OnIssueComment CommentId
  | OnPullRequest PullRequestId
  -- Ideally we would also be able to react to PR reviews, but (as of 9-5-2024) there
  -- doesn't seem to be a REST endpoint for that, despite it being possible through the UI.
  deriving (Show, Eq, Ord, Generic)

instance Buildable ReactableId where
  build (OnIssueComment (CommentId commentId)) =
    "issue comment " <> build commentId
  build (OnPullRequest (PullRequestId prId)) =
    "pull request " <> build prId

instance FromJSON ReactableId

instance ToJSON ReactableId where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
