-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveGeneric #-}

module Configuration
(
  Configuration (..),
  ProjectConfiguration (..),
  TlsConfiguration (..),
  UserConfiguration (..),
  loadConfiguration
)
where

import Data.Aeson (FromJSON, eitherDecodeStrict')
import Data.ByteString (readFile)
import Data.Text (Text)
import GHC.Generics
import Prelude hiding (readFile)

data ProjectConfiguration = ProjectConfiguration
  {
    owner      :: Text,     -- The GitHub user or organization who owns the repo.
    repository :: Text,     -- The name of the repository.
    checkout   :: FilePath, -- The path to a local checkout of the repository.
    buildDir   :: FilePath, -- The directory in which to create build subdirs.
    stateFile  :: FilePath, -- The file where project state is stored.

    -- The build agent binary. Should really be a global option, but this is a
    -- hackathon, so meh.
    builder :: FilePath
  }
  deriving (Generic)

data UserConfiguration = UserConfiguration
  {
    name :: Text,                 -- Name used for Git committer.
    email :: Text,                -- Email address used for Git committer.
    sshConfigFile :: FilePath     -- The path to ~/.ssh/config.
  }
  deriving (Generic)

data TlsConfiguration = TlsConfiguration
  {
    certFile :: FilePath,
    keyFile  :: FilePath
  }
  deriving (Generic, Show)

data Configuration = Configuration
  {
    -- The projects to manage.
    projects :: [ProjectConfiguration],

    -- The secret for GitHub webhook hmac signatures. Note that for webhooks
    -- only it would be better if these were per project, but the GitHub
    -- "integrations" only get one webhook per integration, so in that case
    -- there can be only one secret. (Note that it would be much better if
    -- GitHub were to sign their requests with a public/private key pair, but
    -- alas, that is not the case.)
    secret :: Text,

    -- The port to run the webserver on.
    port :: Int,

    -- Optional config for enabling https.
    tls :: Maybe TlsConfiguration,

    -- Configuration of the Git user.
    user :: UserConfiguration
  }
  deriving (Generic)

instance FromJSON Configuration
instance FromJSON ProjectConfiguration
instance FromJSON TlsConfiguration
instance FromJSON UserConfiguration

-- Reads and parses the configuration. Returns Nothing if parsing failed, but
-- crashes if the file could not be read.
loadConfiguration :: FilePath -> IO (Either String Configuration)
loadConfiguration = fmap eitherDecodeStrict' . readFile
