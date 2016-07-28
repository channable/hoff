-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

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
  WebhookEvent (..),
  eventRepository,
  eventRepositoryOwner,
  newEventQueue
)
where

import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue)
import Control.Monad.STM (atomically)
import Data.Aeson (FromJSON (parseJSON), Object, Value (Object, String), (.:))
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Text (Text)
import Git (Sha (..))

data PullRequestAction
  = Opened
  | Closed
  | Reopened
  | Synchronize
  deriving (Eq, Show)

data CommentAction
  = Created
  | Edited
  | Deleted
  deriving (Eq, Show)

data CommitStatus
  = Pending
  | Success
  | Failure
  | Error
  deriving (Eq, Show)

data PullRequestPayload = PullRequestPayload {
  action     :: PullRequestAction, -- Corresponds to "action".
  owner      :: Text, -- Corresponds to "pull_request.base.repo.owner.login".
  repository :: Text, -- Corresponds to "pull_request.base.repo.name".
  number     :: Int,  -- Corresponds to "pull_request.number".
  sha        :: Sha,  -- Corresponds to "pull_request.head.sha".
  author     :: Text  -- Corresponds to "pull_request.user.login".
} deriving (Eq, Show)

data CommentPayload = CommentPayload {
  action     :: CommentAction, -- Corresponds to "action".
  owner      :: Text, -- Corresponds to "repository.owner.login".
  repository :: Text, -- Corresponds to "repository.name".
  number     :: Int,  -- Corresponds to "issue.number".
  author     :: Text, -- Corresponds to "sender.login".
  body       :: Text  -- Corresponds to "comment.body".
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
  parseJSON _                      = fail "unexpected pull_request action"

instance FromJSON CommentAction where
  parseJSON (String "created") = return Created
  parseJSON (String "edited")  = return Edited
  parseJSON (String "deleted") = return Deleted
  parseJSON _                  = fail "unexpected issue_comment action"

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
    <*> getNested v ["pull_request", "head", "sha"]
    <*> getNested v ["pull_request", "user", "login"]
  parseJSON nonObject = typeMismatch "pull_request payload" nonObject

instance FromJSON CommentPayload where
  parseJSON (Object v) = CommentPayload
    <$> (v .: "action")
    <*> getNested v ["repository", "owner", "login"]
    <*> getNested v ["repository", "name"]
    <*> getNested v ["issue", "number"]
    <*> getNested v ["sender", "login"]
    <*> getNested v ["comment", "body"]
  parseJSON nonObject = typeMismatch "issue_comment payload" nonObject

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

type EventQueue = TBQueue WebhookEvent

-- Creates a new event queue with the given maximum capacity.
newEventQueue :: Int -> IO EventQueue
newEventQueue capacity = atomically $ newTBQueue capacity
