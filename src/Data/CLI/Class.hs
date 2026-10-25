{-# LANGUAGE FunctionalDependencies #-}

-- | Classes implemented by application configs and commands.
module Data.CLI.Class (
  CLIConfig (..),
  Command (..),
) where

import Effectful (Eff)
import Effectful.CLI (CLIEffects)
import Options.Applicative (Parser)

-- | Application config exposing the prompt auto-answer flags.
--
-- @since 0.1.0.0
class CLIConfig a where
  -- | Answer yes to all confirmation prompts.
  autoAccept :: a -> Bool

  -- | Answer no to all confirmation prompts; takes precedence over 'autoAccept'.
  autoDecline :: a -> Bool

-- | A subcommand @a@ running with config @c@ and application error @e@.
--
-- @since 0.1.0.0
class Command c e a | a -> c e where
  -- | Subcommand name on the command line.
  cmdname :: a -> String

  -- | One-line description shown in @--help@.
  doc :: a -> String

  -- | Parser for the subcommand's arguments.
  parser :: a -> Parser a

  -- | Execute the subcommand.
  runner :: CLIEffects c e es => a -> Eff es ()
