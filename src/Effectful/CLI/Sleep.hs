-- | Logged sleeping helpers.
module Effectful.CLI.Sleep (
  msleep,
  sleep,
  sleepUntil,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger (logDebugN, logInfoN)
import Data.Text qualified as T
import Data.Time (
  LocalTime,
  defaultTimeLocale,
  diffLocalTime,
  formatTime,
  nominalDiffTimeToSeconds,
 )
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.CLI.Time (getLocalTime)
import Effectful.Logger (Logger)
import Text.Printf (printf)

-- | Sleep for the given number of milliseconds.
--
-- @since 0.1.0.0
msleep :: IOE :> es => Logger :> es => Int -> Eff es ()
msleep n = do
  logDebugN (T.pack (printf "sleeping for %dms" n))
  liftIO (threadDelay (n * 1000))

-- | Sleep for the given number of seconds.
--
-- @since 0.1.0.0
sleep :: IOE :> es => Logger :> es => Int -> Eff es ()
sleep n = do
  logDebugN (T.pack (printf "sleeping for %ds" n))
  liftIO (threadDelay (n * 1000000))

-- | Sleep until the given local time.
--
-- @since 0.1.0.0
sleepUntil :: IOE :> es => Logger :> es => LocalTime -> Eff es ()
sleepUntil t = do
  logInfoN (T.pack ("sleeping until " <> formatTime defaultTimeLocale "%R" t))
  now <- getLocalTime
  sleep . ceiling . nominalDiffTimeToSeconds $ diffLocalTime t now
