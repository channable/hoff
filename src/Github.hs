-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Github
(
  CommentAction (..),
  CommentPayload (..),
  CommitStatus (..),
  CommitStatusPayload (..),
  EventQueue,
  PullRequestAction (..),
  PullRequestPayload (..),
  ReviewAction (..),
  WebhookEvent (..),
  eventProjectInfo,
  newEventQueue,
  tryEnqueueEvent
)
where

import Control.Applicative ((<|>), optional)
import Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueue, writeTBQueue)
import Control.Monad.STM (atomically)
import Data.Aeson (FromJSON (parseJSON), Object, Value (Object, String), (.:))
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Text (Text)
import GHC.Natural (Natural)

import Git (Sha (..), Branch (..))
import Project (ProjectInfo (..))
import Types (Username)
import Data.Maybe (fromMaybe)

data PullRequestAction
  = Opened
  | Closed
  | Reopened
  | Synchronize
  | Edited
  deriving (Eq, Show)

data CommentAction
  = CommentCreated
  | CommentEdited
  | CommentDeleted
  deriving (Eq, Show)

data ReviewAction
  = ReviewSubmitted
  | ReviewEdited
  | ReviewDismissed
  deriving (Eq, Show)

data CommitStatus
  = Pending
  | Success
  | Failure
  | Error
  deriving (Eq, Show)

data PullRequestPayload = PullRequestPayload {
  action     :: PullRequestAction, -- Corresponds to "action".
  owner      :: Text,     -- Corresponds to "pull_request.base.repo.owner.login".
  repository :: Text,     -- Corresponds to "pull_request.base.repo.name".
  number     :: Int,      -- Corresponds to "pull_request.number".
  branch     :: Branch,   -- Corresponds to "pull_request.head.ref".
  sha        :: Sha,      -- Corresponds to "pull_request.head.sha".
  title      :: Text,     -- Corresponds to "pull_request.title".
  author     :: Username  -- Corresponds to "pull_request.user.login".
} deriving (Eq, Show)

data CommentPayload = CommentPayload {
  action     :: Either CommentAction ReviewAction, -- Corresponds to "action".
  owner      :: Text,     -- Corresponds to "repository.owner.login".
  repository :: Text,     -- Corresponds to "repository.name".
  number     :: Int,      -- Corresponds to "issue.number" or "pull_request.number".
  author     :: Username, -- Corresponds to "sender.login".
  body       :: Text      -- Corresponds to "comment.body" or "review.body".
} deriving (Eq, Show)

data CommitStatusPayload = CommitStatusPayload {
  owner      :: Text,         -- Corresponds to "repository.owner.login".
  repository :: Text,         -- Corresponds to "repository.name".
  status     :: CommitStatus, -- Corresponds to "action".
  url        :: Maybe Text,   -- Corresponds to "target_url".
  sha        :: Sha           -- Corresponds to "sha".
} deriving (Eq, Show)

instance FromJSON PullRequestAction where
  parseJSON (String "opened")      = return Opened
  parseJSON (String "closed")      = return Closed
  parseJSON (String "reopened")    = return Opened
  parseJSON (String "synchronize") = return Synchronize
  parseJSON (String "edited")      = return Edited
  parseJSON _                      = fail "unexpected pull_request action"

instance FromJSON CommentAction where
  parseJSON (String "created")   = return CommentCreated
  parseJSON (String "edited")    = return CommentEdited
  parseJSON (String "deleted")   = return CommentDeleted
  parseJSON _                    = fail "unexpected issue_comment action"

instance FromJSON ReviewAction where
  parseJSON (String "submitted") = return ReviewSubmitted
  parseJSON (String "edited")    = return ReviewEdited
  parseJSON (String "dismissed") = return ReviewDismissed
  parseJSON _                    = fail "unexpected pull_request_review action"

instance FromJSON CommitStatus where
  parseJSON (String "pending") = return Pending
  parseJSON (String "success") = return Success
  parseJSON (String "failure") = return Failure
  parseJSON (String "error")   = return Error
  parseJSON _                  = fail "unexpected status state"

-- A helper function to parse nested fields in json.
getNested :: FromJSON a => Object -> [Text] -> Parser a
getNested rootObject fields =
  -- Build object parsers for every field except the last one. The last field is
  -- different, as it needs a parser of type "a", not "Object".
  let parsers :: [Object -> Parser Object]
      parsers = fmap (\ field -> (.: field)) (init fields)
      object  = foldl (>>=) (return rootObject) parsers
  in  object >>= (.: (last fields))

instance FromJSON PullRequestPayload where
  parseJSON (Object v) = PullRequestPayload
    <$> (v .: "action")
    <*> getNested v ["pull_request", "base", "repo", "owner", "login"]
    <*> getNested v ["pull_request", "base", "repo", "name"]
    <*> getNested v ["pull_request", "number"]
    <*> getNested v ["pull_request", "head", "ref"]
    <*> getNested v ["pull_request", "head", "sha"]
    <*> getNested v ["pull_request", "title"]
    <*> getNested v ["pull_request", "user", "login"]
  parseJSON nonObject = typeMismatch "pull_request payload" nonObject

instance FromJSON CommentPayload where
  parseJSON (Object v) = do
    isReview <- optional (v .: "review" :: Parser Value)
    parsedAction <- case isReview of
      Nothing -> Left <$> v .: "action"
      Just _ -> Right <$> v .: "action"
    CommentPayload parsedAction
      <$> getNested v ["repository", "owner", "login"]
      <*> getNested v ["repository", "name"]
      -- We subscribe to both issue comments and pull request review comments.
      <*> (getNested v ["issue", "number"]
        <|> getNested v ["pull_request", "number"])
      <*> getNested v ["sender", "login"]
      <*> (getNested v ["comment", "body"]
        <|> fromMaybe "" <$> getNested v ["review", "body"])
  parseJSON nonObject = typeMismatch "(issue_comment | pull_request_review) payload" nonObject

instance FromJSON CommitStatusPayload where
  parseJSON (Object v) = CommitStatusPayload
    <$> getNested v ["repository", "owner", "login"]
    <*> getNested v ["repository", "name"]
    <*> (v .: "state")
    <*> (v .: "target_url")
    <*> (v .: "sha")
  parseJSON nonObject = typeMismatch "status payload" nonObject

-- Note that GitHub calls pull requests "issues" for the sake of comments: the
-- pull request comment event is actually "issue_comment".
data WebhookEvent
  = Ping
  | PullRequest PullRequestPayload
  | Comment CommentPayload
  | CommitStatus CommitStatusPayload
  deriving (Eq, Show)

-- Returns the owner of the repository for which the webhook was triggered.
eventRepositoryOwner :: WebhookEvent -> Text
eventRepositoryOwner event = case event of
  Ping                 -> error "ping event must not be processed"
  PullRequest payload  -> owner (payload :: PullRequestPayload)
  Comment payload      -> owner (payload :: CommentPayload)
  CommitStatus payload -> owner (payload :: CommitStatusPayload)

-- Returns the name of the repository for which the webhook was triggered.
eventRepository :: WebhookEvent -> Text
eventRepository event = case event of
  Ping                 -> error "ping event must not be processed"
  PullRequest payload  -> repository (payload :: PullRequestPayload)
  Comment payload      -> repository (payload :: CommentPayload)
  CommitStatus payload -> repository (payload :: CommitStatusPayload)

eventProjectInfo :: WebhookEvent -> ProjectInfo
eventProjectInfo event =
  ProjectInfo (eventRepositoryOwner event) (eventRepository event)

type EventQueue = TBQueue WebhookEvent

-- Creates a new event queue with the given maximum capacity.
newEventQueue :: Natural -> IO EventQueue
newEventQueue capacity = atomically $ newTBQueue capacity

-- Enqueues the event if the queue is not full. Returns whether the event has
-- been enqueued. This function does not block.
tryEnqueueEvent :: EventQueue -> WebhookEvent -> IO Bool
tryEnqueueEvent queue event = atomically $ do
  isFull <- isFullTBQueue queue
  if isFull
    then return False
    -- Normally writeTBQueue would block if the queue is full, but at this point
    -- we know that the queue is not full, so it will return immediately.
    else (writeTBQueue queue event) >> (return True)
