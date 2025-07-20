{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Time (addTime, getDateTime, runTime, TimeOperation (..)) where

import Control.Monad.IO.Class (liftIO)
import Data.Time (DiffTime, UTCTime, addUTCTime, getCurrentTime)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

data TimeOperation :: Effect where
  GetDateTime :: TimeOperation m UTCTime

type instance DispatchOf TimeOperation = 'Dynamic

getDateTime :: TimeOperation :> es => Eff es UTCTime
getDateTime = send GetDateTime

runTime :: IOE :> es => Eff (TimeOperation : es) a -> Eff es a
runTime = interpret $ \_ -> \case
  GetDateTime -> liftIO getCurrentTime

addTime :: UTCTime -> DiffTime -> UTCTime
addTime t d = addUTCTime (realToFrac d) t
