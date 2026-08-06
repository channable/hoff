{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSetup (testProjectConfig, testTriggerConfig, testTimeouts, testTime, fakeRunTime) where

import Data.Time qualified as T
import Data.Time.Calendar.OrdinalDate qualified as T
import Effectful (Eff)
import Effectful.Dispatch.Dynamic (interpret)

import Configuration qualified as Config
import Time (TimeOperation)
import Time qualified

testTriggerConfig :: Config.TriggerConfiguration
testTriggerConfig =
  Config.TriggerConfiguration
    { Config.commentPrefix = "@bot"
    }

testProjectConfig :: Config.ProjectConfiguration
testProjectConfig =
  Config.ProjectConfiguration
    { Config.owner = "peter"
    , Config.repository = "rep"
    , Config.branch = "master"
    , Config.testBranch = "testing"
    , Config.checkout = "/var/lib/hoff/checkouts/peter/rep"
    , Config.stateFile = "/var/lib/hoff/state/peter/rep.json"
    , Config.checks = Just (Config.ChecksConfiguration mempty)
    , Config.deployEnvironments = Just ["staging", "production"]
    , Config.deploySubprojects = Just ["aaa", "bbb"]
    , Config.safeForFriday = Nothing
    , Config.allowPlainMerge = Just True
    }

testTime :: T.UTCTime
testTime = T.UTCTime (T.fromMondayStartWeek 2021 2 1) (T.secondsToDiffTime 0)

testTimeouts :: Config.Timeouts
testTimeouts = Config.Timeouts 600 600 6000

fakeRunTime :: Eff (TimeOperation : es) a -> Eff es a
fakeRunTime = interpret $ \_ -> \case
  Time.GetDateTime -> pure testTime
