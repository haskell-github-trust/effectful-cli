{-# LANGUAGE FunctionalDependencies #-}

module Data.CLI.Class (
  CLIConfig (..),
  Command (..),
) where

import Effectful (Eff)
import Effectful.CLI (CLIEffects)
import Options.Applicative (Parser)

class CLIConfig a where
  autoAccept, autoDecline :: a -> Bool

class Command c e a | a -> c e where
  cmdname :: a -> String
  doc :: a -> String
  parser :: a -> Parser a
  runner :: CLIEffects c e es => a -> Eff es ()
