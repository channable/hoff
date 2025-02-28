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
  knownEnvironments,
  knownSubprojects,
  ChecksConfiguration (..),
  TlsConfiguration (..),
  TriggerConfiguration (..),
  UserConfiguration (..),
  MergeWindowExemptionConfiguration (..),
  MetricsConfiguration (..),
  FeatureFreezeWindow (..),
  Timeouts (..),
  ClockTickInterval (..),
  loadConfiguration
)
where

import Data.Aeson (FromJSON, eitherDecodeStrict')
import Data.ByteString (readFile)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (DiffTime, UTCTime)
import GHC.Generics
import Prelude hiding (readFile)
import qualified Network.Wai.Handler.Warp as Warp

data ProjectConfiguration = ProjectConfiguration
  {
    owner              :: Text,                      -- The GitHub user or organization who owns the repo.
    repository         :: Text,                      -- The name of the repository.
    branch             :: Text,                      -- The branch to guard and integrate commits into.
    testBranch         :: Text,                      -- The branch to force-push candidates to for testing.
    checkout           :: FilePath,                  -- The path to a local checkout of the repository.
    stateFile          :: FilePath,                  -- The file where project state is stored.
    checks             :: Maybe ChecksConfiguration, -- Optional configuration related to checks for the project.
    deployEnvironments :: Maybe [Text],              -- The environments which the `deploy to <environment>` command should be enabled for
    deploySubprojects  :: Maybe [Text],              -- The subprojects which the `deploy` command should be enabled for
    safeForFriday      :: Maybe Bool                 -- Whether it's safe to deploy this project on Friday without an "on Friday" check. default False
  }
  deriving (Generic)

knownEnvironments :: ProjectConfiguration -> [Text]
knownEnvironments = fromMaybe [] . deployEnvironments

knownSubprojects :: ProjectConfiguration -> [Text]
knownSubprojects = fromMaybe [] . deploySubprojects

data FeatureFreezeWindow = FeatureFreezeWindow
  {
    start :: UTCTime,
    end   :: UTCTime
  }
  deriving (Generic)

data TriggerConfiguration = TriggerConfiguration
  {
    -- When a comment with this prefix is left on a PR, that triggers the
    -- remainder of the comment to be interpreted as a directive at the bot.
    -- Usually this would be the Github username of the bot (including @ but not
    -- a space), e.g. "@hoffbot", and the comment "@hoffbot merge" would trigger
    -- a merge. The prefix is case-insensitive.
    commentPrefix :: Text
  }
  deriving (Generic)

data UserConfiguration = UserConfiguration
  {
    name :: Text,                 -- Name used for Git committer.
    email :: Text,                -- Email address used for Git committer.
    sshConfigFile :: FilePath     -- The path to ~/.ssh/config.
  }
  deriving (Generic)

newtype ChecksConfiguration = ChecksConfiguration
  {
    -- Multiple checks can succeed on a PR, the normal behaviour is to merge
    -- once the first one succeeds. Instead, we make it possible to configure a
    -- list of mandatory checks that should have succesfully finished before we
    -- allow the merge. Each entry is a prefix of the context that sent the
    -- status update.
    mandatory :: Set Text
  }
  deriving (Generic)

data TlsConfiguration = TlsConfiguration
  {
    certFile :: FilePath,
    keyFile  :: FilePath
  }
  deriving (Generic, Show)

data MetricsConfiguration = MetricsConfiguration
  {
    metricsPort :: Warp.Port,
    metricsHost :: Text
  }
  deriving (Generic, Show)

newtype MergeWindowExemptionConfiguration = MergeWindowExemptionConfiguration [Text]
  deriving (Generic, Show)

data Timeouts = Timeouts
  {
    promotionTimeout :: DiffTime,
    rememberTimeout :: DiffTime
  }
  deriving (Generic, Show)

newtype ClockTickInterval = ClockTickInterval DiffTime
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

    -- The access token for the Github API, for leaving comments.
    accessToken :: Text,

    -- Triggers that the bot may respond to.
    trigger :: TriggerConfiguration,

    -- The port to run the webserver on.
    port :: Int,

    -- Optional config for enabling https.
    tls :: Maybe TlsConfiguration,

    -- Configuration of the Git user.
    user :: UserConfiguration,

    -- List of users that are exempted from the merge window. This is useful for
    -- bots that automatically merge low impact changes.
    mergeWindowExemption :: MergeWindowExemptionConfiguration,

    -- Configuration for the Prometheus metrics server.
    metricsConfig :: Maybe MetricsConfiguration,

    -- Feature freeze period in which only 'merge hotfix' commands are allowed
    featureFreezeWindow :: Maybe FeatureFreezeWindow,

    -- The timeouts for promoting an integrated pull request and remembering promoted pull requests
    timeouts :: Timeouts,

    -- The interval to send clock tick events
    clockTickInterval :: Maybe ClockTickInterval
  }
  deriving (Generic)

instance FromJSON Configuration
instance FromJSON ChecksConfiguration
instance FromJSON ProjectConfiguration
instance FromJSON TlsConfiguration
instance FromJSON TriggerConfiguration
instance FromJSON UserConfiguration
instance FromJSON MergeWindowExemptionConfiguration
instance FromJSON MetricsConfiguration
instance FromJSON FeatureFreezeWindow
instance FromJSON Timeouts
instance FromJSON ClockTickInterval

-- Reads and parses the configuration. Returns Nothing if parsing failed, but
-- crashes if the file could not be read.
loadConfiguration :: FilePath -> IO (Either String Configuration)
loadConfiguration = fmap eitherDecodeStrict' . readFile
