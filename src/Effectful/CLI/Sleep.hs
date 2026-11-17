-- | Logged sleeping helpers.
module Effectful.CLI.Sleep (
  msleep,
  sleep,
  sleepFor,
  sleepUntil,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger (logDebugN, logInfoN)
import Data.Text qualified as T
import Data.Time (
  LocalTime,
  NominalDiffTime,
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
msleep
  :: IOE :> es
  => Logger :> es
  => Int
  -- ^ milliseconds
  -> Eff es ()
msleep n = do
  logDebugN (T.pack (printf "sleeping for %dms" n))
  liftIO (threadDelay (n * 1000))

-- | Sleep for the given number of seconds.
--
-- @since 0.1.0.0
sleep
  :: IOE :> es
  => Logger :> es
  => Int
  -- ^ seconds
  -> Eff es ()
sleep n = do
  logDebugN (T.pack (printf "sleeping for %ds" n))
  liftIO (threadDelay (n * 1000000))

-- | Sleep until the given local time.
--
-- @since 0.1.0.0
sleepUntil
  :: IOE :> es
  => Logger :> es
  => LocalTime
  -- ^ wake-up time
  -> Eff es ()
sleepUntil t = do
  logInfoN (T.pack ("sleeping until " <> formatTime defaultTimeLocale "%R" t))
  now <- getLocalTime
  sleepFor (diffLocalTime t now)

-- | Sleep for the given duration.
sleepFor
  :: IOE :> es
  => Logger :> es
  => NominalDiffTime
  -> Eff es ()
sleepFor d = do
  logDebugN (T.pack ("sleeping for " <> show d))
  liftIO (threadDelay (max 0 (ceiling (nominalDiffTimeToSeconds d * 1000000))))
