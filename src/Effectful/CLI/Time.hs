-- | Current time helpers, in 'IO' and 'Eff'.
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

-- | Current time in the local time zone.
--
-- @since 0.1.0.0
getLocalTimeIO :: IO LocalTime
getLocalTimeIO = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

-- | Midnight UTC of the current day.
--
-- @since 0.1.0.0
getBeginningOfDayIO :: IO UTCTime
getBeginningOfDayIO = (\now -> now{utctDayTime = 0}) <$> getCurrentTime

-- | Midnight UTC of the Monday of the current week.
--
-- @since 0.1.0.0
getBeginningOfWeekIO :: IO UTCTime
getBeginningOfWeekIO = (\now -> UTCTime (weekFirstDay Monday (utctDay now)) 0) <$> getCurrentTime

-- | Current time in the local time zone.
--
-- @since 0.1.0.0
getLocalTime :: IOE :> es => Eff es LocalTime
getLocalTime = liftIO getLocalTimeIO

-- | Midnight UTC of the current day.
--
-- @since 0.1.0.0
getBeginningOfDay :: IOE :> es => Eff es UTCTime
getBeginningOfDay = liftIO getBeginningOfDayIO

-- | Midnight UTC of the Monday of the current week.
--
-- @since 0.1.0.0
getBeginningOfWeek :: IOE :> es => Eff es UTCTime
getBeginningOfWeek = liftIO getBeginningOfWeekIO
