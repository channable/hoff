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
  increaseMergeFailedPRTotal,
  increaseMergedPRTotal,
  updateTrainSizeGauge,
  registerGHCMetrics,
  registerProjectMetrics,
  priorityLabelValue,
  failReasonLabelValue,
)
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Prometheus
import Prometheus.Metric.GHC (ghcMetrics)

import Git (GitIntegrationFailure (..))
import Project (Priority (..))

type ProjectLabel = Text
type PriorityLabel = Text
type ReasonLabel = Text

data ProjectMetrics = ProjectMetrics
  { projectMetricsMergedPR :: Vector (ProjectLabel, PriorityLabel) Counter
  , projectMetricsMergeAttemptedPR :: Vector (ProjectLabel, PriorityLabel) Counter
  , projectMetricsMergeFailedPR :: Vector (ProjectLabel, PriorityLabel, ReasonLabel) Counter
  , projectMetricsMergeTrainSize :: Vector ProjectLabel Gauge
  }

data MetricsOperation :: Effect where
  MergeBranch :: PriorityLabel -> MetricsOperation m ()
  MergeAttemptedBranch :: PriorityLabel -> MetricsOperation m ()
  MergeFailedBranch :: PriorityLabel -> ReasonLabel -> MetricsOperation m ()
  UpdateTrainSize :: Int -> MetricsOperation m ()

type instance DispatchOf MetricsOperation = 'Dynamic

increaseMergedPRTotal :: MetricsOperation :> es => PriorityLabel -> Eff es ()
increaseMergedPRTotal priority = send $ MergeBranch priority

increaseMergeAttemptedPRTotal :: MetricsOperation :> es => PriorityLabel -> Eff es ()
increaseMergeAttemptedPRTotal priority = send $ MergeAttemptedBranch priority

increaseMergeFailedPRTotal :: MetricsOperation :> es => PriorityLabel -> ReasonLabel -> Eff es ()
increaseMergeFailedPRTotal priority reason = send $ MergeFailedBranch priority reason

priorityLabelValue :: Priority -> PriorityLabel
priorityLabelValue Normal = "normal"
priorityLabelValue High = "high"

failReasonLabelValue :: GitIntegrationFailure -> ReasonLabel
failReasonLabelValue MergeFailed = "merge_failed"
failReasonLabelValue RebaseFailed = "rebase_failed"
failReasonLabelValue WrongFixups = "wrong_fixups"
failReasonLabelValue EmptyRebase = "empty_rebase"
failReasonLabelValue (FailedForcePush _) = "failed_force_push"

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
  MergeFailedBranch priority reason ->
    void $
      liftIO $
        incProjectMergeFailedPR metrics label priority reason

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
          ("project", "priority", "reason")
          ( counter
              ( Info
                  "hoff_project_merge_failed_pull_requests"
                  "Number of pull request merges that failed"
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

incProjectMergeFailedPR :: ProjectMetrics -> ProjectLabel -> PriorityLabel -> ReasonLabel -> IO ()
incProjectMergeFailedPR metrics project priority reason =
  withLabel (projectMetricsMergeFailedPR metrics) (project, priority, reason) incCounter

setProjectMetricMergeTrainSize :: ProjectMetrics -> ProjectLabel -> Int -> IO ()
setProjectMetricMergeTrainSize metrics project n =
  withLabel (projectMetricsMergeTrainSize metrics) project (\g -> setGauge g (fromIntegral n))
