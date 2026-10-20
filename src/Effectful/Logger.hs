{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | monad-logger bridge: a static effect holding a log callback, plus a
-- 'MonadLogger' instance for 'Eff' so the usual @logInfoN@ etc. work.
module Effectful.Logger (
  Logger,
  LogFunc,
  runLogger,
  runStderrLogger,
  runOTelLogger,
) where

import Control.Monad.Logger (
  Loc,
  LogLevel,
  LogSource,
  LogStr,
  MonadLogger (..),
  defaultOutput,
  toLogStr,
 )
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (
  SideEffects (..),
  StaticRep,
  evalStaticRep,
  getStaticRep,
  unsafeEff_,
 )
import OpenTelemetry.Instrumentation.MonadLogger qualified as OTel
import OpenTelemetry.Log.Core qualified as OTel
import System.IO (stderr)

-- | @since 0.1.0.0
type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | @since 0.1.0.0
data Logger :: Effect

type instance DispatchOf Logger = Static WithSideEffects

newtype instance StaticRep Logger = Logger LogFunc

-- | @since 0.1.0.0
runLogger :: IOE :> es => LogFunc -> Eff (Logger : es) a -> Eff es a
runLogger = evalStaticRep . Logger

-- | @since 0.1.0.0
runStderrLogger :: IOE :> es => Eff (Logger : es) a -> Eff es a
runStderrLogger = runLogger (defaultOutput stderr)

-- | @since 0.1.0.0
runOTelLogger :: IOE :> es => OTel.Logger -> Eff (Logger : es) a -> Eff es a
runOTelLogger = runLogger . OTel.makeOTelLogCallback

instance Logger :> es => MonadLogger (Eff es) where
  monadLoggerLog loc src lvl msg = do
    Logger f <- getStaticRep
    unsafeEff_ $ f loc src lvl (toLogStr msg)
