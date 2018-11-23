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
  ProjectInfo (..),
  ProjectState (..),
  Push (..),
  emptyProjectState,
  insertPush,
  loadProjectState,
  pushesFailed,
  pushesPending,
  pushesStarted,
  pushesSucceeded,
  saveProjectState,
  setBuildStatus,
)
where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericToEncoding)
import Data.ByteString (readFile)
import Data.ByteString.Lazy (writeFile)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Generics (Generic)
import Git (Branch (..), Sha (..))
import Prelude hiding (readFile, writeFile)
import System.Directory (renameFile)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.HashMap.Strict as HashMap

data BuildStatus
  = Pending
  | Started
  | Succeeded
  | Failed
  deriving (Eq, Generic, Ord, Show)

data Push = Push
  {
    -- The new HEAD of the branch.
    pushSha    :: Sha,
    -- The ref pushed to, e.g. "refs/heads/master".
    pushRef    :: Branch,
    -- Summary line of the head commit.
    pushTitle  :: Text,
    -- Author of the head commit.
    pushAuthor :: Text
  }
  deriving (Eq, Generic, Show)

data ProjectState = ProjectState
  {
    projectPushes :: [Push],
    projectBuilds :: HashMap Sha BuildStatus
  }
  deriving (Eq, Show, Generic)

-- Static information about a project, which does not change while the program
-- is running.
data ProjectInfo = ProjectInfo
  {
    owner      :: Text,
    repository :: Text
  }
  deriving (Eq, Show)

-- TODO: These default instances produce ugly json. Write a custom
-- implementation. For now this will suffice.
instance FromJSON BuildStatus
instance FromJSON ProjectState
instance FromJSON Push

instance ToJSON BuildStatus where toEncoding = genericToEncoding defaultOptions
instance ToJSON ProjectState where toEncoding = genericToEncoding defaultOptions
instance ToJSON Push where toEncoding = genericToEncoding defaultOptions

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
  projectPushes = [],
  projectBuilds = HashMap.empty
}

-- Inserts a new push into the project.
insertPush :: Push -> ProjectState -> ProjectState
insertPush push state =
  state {
    projectPushes = push : projectPushes state,
    projectBuilds = HashMap.insertWith (\ _ old -> old) (pushSha push) Pending $ projectBuilds state
  }

setBuildStatus :: Sha -> BuildStatus -> ProjectState -> ProjectState
setBuildStatus sha status state =
  state {
    projectBuilds = HashMap.adjust (const status) sha $ projectBuilds state
  }

pushesWithStatus :: ProjectState -> [(Push, BuildStatus)]
pushesWithStatus state =
  let
    get sha = case HashMap.lookup sha (projectBuilds state) of
      Just status -> status
      Nothing -> error "Invariant violated: all pushes must have a build status."
    withStatus push = (push, get $ pushSha push)
  in
    fmap withStatus $ projectPushes state

filterPushesByStatus :: (BuildStatus -> Bool) -> ProjectState -> [Push]
filterPushesByStatus p = fmap fst . filter (p . snd) . pushesWithStatus

pushesPending :: ProjectState -> [Push]
pushesPending = filterPushesByStatus (== Pending)

pushesStarted :: ProjectState -> [Push]
pushesStarted = filterPushesByStatus (== Started)

pushesSucceeded :: ProjectState -> [Push]
pushesSucceeded = filterPushesByStatus (== Succeeded)

pushesFailed :: ProjectState -> [Push]
pushesFailed = filterPushesByStatus (== Failed)
