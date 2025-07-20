module ClockTickLoop (
  clockTickLoop,
) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (getCurrentTime)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)

import Configuration (ClockTickInterval (..))
import Logic (Event (ClockTick), EventQueue, enqueueEvent)

foreverWithDelay :: MonadIO m => DiffTime -> m a -> m b
foreverWithDelay delay action = go
 where
  go = do
    void $ action
    liftIO $ threadDelay $ fromInteger $ diffTimeToMicroseconds delay
    go

-- | Creates a loop that every interval sends a clock tick event
-- to check for timed out PRs that are waiting to be promoted.
clockTickLoop
  :: MonadIO m
  => ClockTickInterval
  -> [EventQueue]
  -> m a
clockTickLoop (ClockTickInterval tickInterval) queues = do
  foreverWithDelay tickInterval $ do
    currentTime <- liftIO getCurrentTime
    liftIO $ mapM_ (`enqueueEvent` ClockTick currentTime) queues

-- A picosecond is 1e-12 seconds, a microsecond is 1e-6 seconds.
diffTimeToMicroseconds :: DiffTime -> Integer
diffTimeToMicroseconds t = diffTimeToPicoseconds t `div` (1000 * 1000)
