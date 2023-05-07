{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Metrics.Metrics
(
  MetricsOperation (..),
  ProjectMetrics (..),
  runMetrics,
  increaseMergedPRTotal,
  updateTrainSizeGauge,
  registerGHCMetrics,
  registerProjectMetrics
  )
where

import Data.Text
import Prometheus
import Prometheus.Metric.GHC (ghcMetrics)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

type ProjectLabel = Text

data ProjectMetrics = ProjectMetrics
  { projectMetricsMergedPR :: Vector ProjectLabel Counter
  , projectMetricsMergeTrainSize :: Vector ProjectLabel Gauge
  }

data MetricsOperation :: Effect where
  MergeBranch :: MetricsOperation m ()
  UpdateTrainSize :: Int -> MetricsOperation m ()

type instance DispatchOf MetricsOperation = 'Dynamic

increaseMergedPRTotal :: MetricsOperation :> es => Eff es ()
increaseMergedPRTotal = send MergeBranch

updateTrainSizeGauge :: MetricsOperation :> es => Int -> Eff es ()
updateTrainSizeGauge n = send $ UpdateTrainSize n

runMetrics
  :: IOE :> es
  => ProjectMetrics
  -> ProjectLabel
  -> Eff (MetricsOperation : es) a
  -> Eff es a
runMetrics metrics label = interpret $ \_ -> \case
  UpdateTrainSize n -> void $
    liftIO $ setProjectMetricMergeTrainSize metrics label n
  MergeBranch -> void $
    liftIO $ incProjectMergedPR metrics label

registerGHCMetrics :: IO ()
registerGHCMetrics = void $ register ghcMetrics

registerProjectMetrics :: IO ProjectMetrics
registerProjectMetrics = ProjectMetrics
  <$> register (vector "project" (counter (Info "hoff_project_merged_pull_requests"
                                                 "Number of merged pull requests")))
  <*> register (vector "project" (gauge (Info "hoff_project_merge_train_size"
                                                "Number of pull requests currently in the queue (merge train)")))

incProjectMergedPR :: ProjectMetrics -> ProjectLabel -> IO ()
incProjectMergedPR metrics project =
  withLabel (projectMetricsMergedPR metrics) project incCounter

setProjectMetricMergeTrainSize :: ProjectMetrics -> ProjectLabel -> Int -> IO ()
setProjectMetricMergeTrainSize metrics project n =
  withLabel (projectMetricsMergeTrainSize metrics) project (\g -> setGauge g (fromIntegral n))
