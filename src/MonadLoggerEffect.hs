{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
-- Needed for 'MonadLogger (Eff es)'
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MonadLoggerEffect (MonadLoggerEffect (..), runLoggerStdout) where

import Control.Monad.Logger (
  Loc,
  LogLevel,
  LogSource,
  MonadLogger (..),
  ToLogStr (toLogStr),
  defaultOutput,
 )
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import System.IO (stdout)

data MonadLoggerEffect :: Effect where
  MonadLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> MonadLoggerEffect m ()

type instance DispatchOf MonadLoggerEffect = 'Dynamic

-- This orphan instance allows using monad-logger functions in Eff. UndecidableInstances is needed
-- here because the 'MonadLoggerEffect :> es' constraint actually makes the instance head bigger,
-- rather than smaller. The actual reducing of the es variable happens in the interpretation of the
-- MonadLoggerEffect. See e.g. the runLoggerStdout, which removes the MonadLoggerEffect from the
-- effects list.
instance MonadLoggerEffect :> es => MonadLogger (Eff es) where
  monadLoggerLog loc logSource logLevel msg = send $ MonadLoggerLog loc logSource logLevel msg

-- | Run the logger such that everything is logged to stdout
runLoggerStdout :: IOE :> es => Eff (MonadLoggerEffect : es) a -> Eff es a
runLoggerStdout = interpret $ \_ -> \case
  MonadLoggerLog loc logSource logLevel msg ->
    liftIO $ defaultOutput stdout loc logSource logLevel $ toLogStr msg
