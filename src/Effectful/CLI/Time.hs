module Effectful.CLI.Time (
  getLocalTimeIO,
  getBeginningOfDayIO,
  getBeginningOfWeekIO,
  getLocalTime,
  getBeginningOfDay,
  getBeginningOfWeek,
) where

import Data.Time
import Effectful (Eff, IOE, liftIO, (:>))

getLocalTimeIO :: IO LocalTime
getLocalTimeIO = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

getBeginningOfDayIO :: IO UTCTime
getBeginningOfDayIO = (\now -> now{utctDayTime = 0}) <$> getCurrentTime

getBeginningOfWeekIO :: IO UTCTime
getBeginningOfWeekIO = (\now -> UTCTime (weekFirstDay Monday (utctDay now)) 0) <$> getCurrentTime

getLocalTime :: IOE :> es => Eff es LocalTime
getLocalTime = liftIO getLocalTimeIO

getBeginningOfDay :: IOE :> es => Eff es UTCTime
getBeginningOfDay = liftIO getBeginningOfDayIO

getBeginningOfWeek :: IOE :> es => Eff es UTCTime
getBeginningOfWeek = liftIO getBeginningOfWeekIO
