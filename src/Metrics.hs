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
type PriorityLabel = Text

data ProjectMetrics = ProjectMetrics
  { projectMetricsMergedPR :: Vector (ProjectLabel, PriorityLabel) Counter
  , projectMetricsMergeAttemptedPR :: Vector (ProjectLabel, PriorityLabel) Counter
  , projectMetricsMergeTrainSize :: Vector ProjectLabel Gauge
  }

data MetricsOperation :: Effect where
  MergeBranch :: PriorityLabel -> MetricsOperation m ()
  MergeAttemptedBranch :: PriorityLabel -> MetricsOperation m ()
  UpdateTrainSize :: Int -> MetricsOperation m ()

type instance DispatchOf MetricsOperation = 'Dynamic

increaseMergedPRTotal :: MetricsOperation :> es => PriorityLabel -> Eff es ()
increaseMergedPRTotal priority = send $ MergeBranch priority

increaseMergeAttemptedPRTotal :: MetricsOperation :> es => PriorityLabel -> Eff es ()
increaseMergeAttemptedPRTotal priority = send $ MergeAttemptedBranch priority

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
  MergeBranch priority ->
    void $
      liftIO $
        incProjectMergedPR metrics label priority
  MergeAttemptedBranch priority ->
    void $
      liftIO $
        incProjectMergeAttemptedPR metrics label priority

registerGHCMetrics :: IO ()
registerGHCMetrics = void $ register ghcMetrics

registerProjectMetrics :: IO ProjectMetrics
registerProjectMetrics =
  ProjectMetrics
    <$> register
      ( vector
          ("project", "priority")
          ( counter
              ( Info
                  "hoff_project_merged_pull_requests"
                  "Number of merged pull requests"
              )
          )
      )
    <*> register
      ( vector
          ("project", "priority")
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
          ( gauge
              ( Info
                  "hoff_project_merge_train_size"
                  "Number of pull requests currently in the queue (merge train)"
              )
          )
      )

incProjectMergedPR :: ProjectMetrics -> ProjectLabel -> PriorityLabel -> IO ()
incProjectMergedPR metrics project priority =
  withLabel (projectMetricsMergedPR metrics) (project, priority) incCounter

incProjectMergeAttemptedPR :: ProjectMetrics -> ProjectLabel -> PriorityLabel -> IO ()
incProjectMergeAttemptedPR metrics project priority =
  withLabel (projectMetricsMergeAttemptedPR metrics) (project, priority) incCounter

setProjectMetricMergeTrainSize :: ProjectMetrics -> ProjectLabel -> Int -> IO ()
setProjectMetricMergeTrainSize metrics project n =
  withLabel (projectMetricsMergeTrainSize metrics) project (\g -> setGauge g (fromIntegral n))
