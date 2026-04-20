{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Metrics (
  MetricsOperation (..),
  ProjectMetrics (..),
  runMetrics,
  increaseMergeAttemptedPRTotal,
  increasePriorityMergeAttemptedPRTotal,
  increasePriorityMergedPRTotal,
  increaseMergedPRTotal,
  updateTrainSizeGauge,
  registerGHCMetrics,
  registerProjectMetrics,
)
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Prometheus
import Prometheus.Metric.GHC (ghcMetrics)

type ProjectLabel = Text

data ProjectMetrics = ProjectMetrics
  { projectMetricsMergedPR :: Vector ProjectLabel Counter
  , projectMetricsMergeAttemptedPR :: Vector ProjectLabel Counter
  , projectMetricsPriorityMergeAttemptedPR :: Vector ProjectLabel Counter
  , projectMetricsPriorityMergedPR :: Vector ProjectLabel Counter
  , projectMetricsMergeTrainSize :: Vector ProjectLabel Gauge
  }

data MetricsOperation :: Effect where
  MergeBranch :: MetricsOperation m ()
  MergeAttemptedBranch :: MetricsOperation m ()
  PriorityMergeAttemptedBranch :: MetricsOperation m ()
  PriorityMergeBranch :: MetricsOperation m ()
  UpdateTrainSize :: Int -> MetricsOperation m ()

type instance DispatchOf MetricsOperation = 'Dynamic

increaseMergedPRTotal :: MetricsOperation :> es => Eff es ()
increaseMergedPRTotal = send MergeBranch

increaseMergeAttemptedPRTotal :: MetricsOperation :> es => Eff es ()
increaseMergeAttemptedPRTotal = send MergeAttemptedBranch

increasePriorityMergeAttemptedPRTotal :: MetricsOperation :> es => Eff es ()
increasePriorityMergeAttemptedPRTotal = send PriorityMergeAttemptedBranch

increasePriorityMergedPRTotal :: MetricsOperation :> es => Eff es ()
increasePriorityMergedPRTotal = send PriorityMergeBranch

updateTrainSizeGauge :: MetricsOperation :> es => Int -> Eff es ()
updateTrainSizeGauge n = send $ UpdateTrainSize n

runMetrics
  :: IOE :> es
  => ProjectMetrics
  -> ProjectLabel
  -> Eff (MetricsOperation : es) a
  -> Eff es a
runMetrics metrics label = interpret $ \_ -> \case
  UpdateTrainSize n ->
    void $
      liftIO $
        setProjectMetricMergeTrainSize metrics label n
  MergeBranch ->
    void $
      liftIO $
        incProjectMergedPR metrics label
  MergeAttemptedBranch ->
    void $
      liftIO $
        incProjectMergeAttemptedPR metrics label
  PriorityMergeAttemptedBranch ->
    void $
      liftIO $
        incProjectPriorityMergeAttemptedPR metrics label
  PriorityMergeBranch ->
    void $
      liftIO $
        incProjectPriorityMergedPR metrics label

registerGHCMetrics :: IO ()
registerGHCMetrics = void $ register ghcMetrics

registerProjectMetrics :: IO ProjectMetrics
registerProjectMetrics =
  ProjectMetrics
    <$> register
      ( vector
          "project"
          ( counter
              ( Info
                  "hoff_project_merged_pull_requests"
                  "Number of merged pull requests"
              )
          )
      )
    <*> register
      ( vector
          "project"
          ( counter
              ( Info
                  "hoff_project_merge_attempted_pull_requests"
                  "Number of pull request merges attempted"
              )
          )
      )
    <*> register
      ( vector
          "project"
          ( counter
              ( Info
                  "hoff_project_priority_merge_attempted_pull_requests"
                  "Number of priority pull request merges attempted"
              )
          )
      )
    <*> register
      ( vector
          "project"
          ( counter
              ( Info
                  "hoff_project_priority_merged_pull_requests"
                  "Number of merged priority pull requests"
              )
          )
      )
    <*> register
      ( vector
          "project"
          ( gauge
              ( Info
                  "hoff_project_merge_train_size"
                  "Number of pull requests currently in the queue (merge train)"
              )
          )
      )

incProjectMergedPR :: ProjectMetrics -> ProjectLabel -> IO ()
incProjectMergedPR metrics project =
  withLabel (projectMetricsMergedPR metrics) project incCounter

incProjectMergeAttemptedPR :: ProjectMetrics -> ProjectLabel -> IO ()
incProjectMergeAttemptedPR metrics project =
  withLabel (projectMetricsMergeAttemptedPR metrics) project incCounter

incProjectPriorityMergeAttemptedPR :: ProjectMetrics -> ProjectLabel -> IO ()
incProjectPriorityMergeAttemptedPR metrics project =
  withLabel (projectMetricsPriorityMergeAttemptedPR metrics) project incCounter

incProjectPriorityMergedPR :: ProjectMetrics -> ProjectLabel -> IO ()
incProjectPriorityMergedPR metrics project =
  withLabel (projectMetricsPriorityMergedPR metrics) project incCounter

setProjectMetricMergeTrainSize :: ProjectMetrics -> ProjectLabel -> Int -> IO ()
setProjectMetricMergeTrainSize metrics project n =
  withLabel (projectMetricsMergeTrainSize metrics) project (\g -> setGauge g (fromIntegral n))
