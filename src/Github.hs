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
  CommitStatus (..),
  CommitStatusPayload (..),
  Commit (..),
  EventQueue,
  PushPayload (..),
  WebhookEvent (..),
  eventProjectInfo,
  newEventQueue,
  tryEnqueueEvent
)
where

import Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueue, writeTBQueue)
import Control.Monad.STM (atomically)
import Data.Aeson (FromJSON (parseJSON), Object, Value (Object, String), (.:))
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Text (Text)
import Git (Sha (..), Branch (..))
import Project (ProjectInfo (..))

data CommitStatus
  = Pending
  | Success
  | Failure
  | Error
  deriving (Eq, Show)

data PushPayload = PushPayload {
  owner      :: Text,    -- Corresponds to "repository.name".
  repository :: Text,    -- Corresponds to "repository.owner.name".
  branch     :: Branch,  -- Corresponds to "ref".
  sha        :: Sha,     -- Corresponds to "after".
  commits    :: [Commit] -- Corresponds to "commits".
} deriving (Eq, Show)

data Commit = Commit {
  sha         :: Sha,
  message     :: Text,
  authorName  :: Text,
  authorEmail :: Text
} deriving (Eq, Show)

data CommitStatusPayload = CommitStatusPayload {
  owner      :: Text,         -- Corresponds to "repository.owner.login".
  repository :: Text,         -- Corresponds to "repository.name".
  status     :: CommitStatus, -- Corresponds to "action".
  url        :: Maybe Text,   -- Corresponds to "target_url".
  sha        :: Sha           -- Corresponds to "sha".
} deriving (Eq, Show)

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

instance FromJSON PushPayload where
  parseJSON (Object v) = PushPayload
    <$> getNested v ["repository", "owner", "name"]
    <*> getNested v ["repository", "name"]
    <*> (v .: "ref")
    <*> (v .: "after")
    <*> (v .: "commits")
  parseJSON nonObject = typeMismatch "push payload" nonObject

instance FromJSON Commit where
  parseJSON (Object v) = Commit
    <$> (v .: "id")
    <*> (v .: "message")
    <*> getNested v ["author", "name"]
    <*> getNested v ["author", "email"]
  parseJSON nonObject = typeMismatch "commit" nonObject

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
  | Push PushPayload
  | CommitStatus CommitStatusPayload
  deriving (Eq, Show)

-- Returns the owner of the repository for which the webhook was triggered.
eventRepositoryOwner :: WebhookEvent -> Text
eventRepositoryOwner event = case event of
  Ping                 -> error "ping event must not be processed"
  Push payload         -> owner (payload :: PushPayload)
  CommitStatus payload -> owner (payload :: CommitStatusPayload)

-- Returns the name of the repository for which the webhook was triggered.
eventRepository :: WebhookEvent -> Text
eventRepository event = case event of
  Ping                 -> error "ping event must not be processed"
  Push payload         -> repository (payload :: PushPayload)
  CommitStatus payload -> repository (payload :: CommitStatusPayload)

eventProjectInfo :: WebhookEvent -> ProjectInfo
eventProjectInfo event =
  ProjectInfo (eventRepositoryOwner event) (eventRepository event)

type EventQueue = TBQueue WebhookEvent

-- Creates a new event queue with the given maximum capacity.
newEventQueue :: Int -> IO EventQueue
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
