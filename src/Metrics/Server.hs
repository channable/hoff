module Metrics.Server (
  MetricsServerConfig (..),
  serverConfig,
  runMetricsServer,
)
where

import Data.Function ((&))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Prometheus qualified as PrometheusWai

data MetricsServerConfig = MetricsServerConfig
  { metricsConfigHost :: Warp.HostPreference
  , metricsConfigPort :: Warp.Port
  }

serverConfig :: MetricsServerConfig -> Warp.Settings
serverConfig config =
  Warp.defaultSettings
    & Warp.setHost (metricsConfigHost config)
    & Warp.setPort (metricsConfigPort config)

runMetricsServer :: MetricsServerConfig -> IO ()
runMetricsServer config = Warp.runSettings (serverConfig config) PrometheusWai.metricsApp
