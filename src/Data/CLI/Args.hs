-- | Global command-line options shared by all commands.
module Data.CLI.Args (
  CLIArgs (..),
  getLogLevel,
  showVersion,
  versionOrCommand,
  argParser,
) where

import Control.Monad.Logger (LogLevel (..))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Options.Applicative

-- | Parsed global options, with the command of type @c@.
--
-- @since 0.1.0.0
data CLIArgs c = CLIArgs
  { _logLevel :: Maybe LogLevel
  -- ^ @-l@/@--log-level@
  , _debug :: Bool
  -- ^ @-d@/@--debug@
  , _autoAccept :: Bool
  -- ^ @-y@/@--yes@
  , _autoDecline :: Bool
  -- ^ @-n@/@--no@
  , _config :: Maybe FilePath
  -- ^ @-c@/@--config@
  , _command :: Either () c
  -- ^ 'Left' for @-v@/@--version@, otherwise the parsed command
  }

logLevels :: [(String, LogLevel)]
logLevels =
  [ ("debug", LevelDebug)
  , ("info", LevelInfo)
  , ("warn", LevelWarn)
  , ("error", LevelError)
  ]

-- | Effective log level:
--   * @--log-level@ if given
--   * debug with @--debug@
--   * info otherwise
--
-- @since 0.1.0.0
getLogLevel :: CLIArgs c -> LogLevel
getLogLevel a = fromMaybe (if _debug a then LevelDebug else LevelInfo) (_logLevel a)

-- | Print the version string to stdout.
--
-- @since 0.1.0.0
showVersion :: String -> IO ()
showVersion = putStrLn

-- | Print the version if @--version@ was passed, otherwise run the parsed command.
--
-- @since 0.1.0.0
versionOrCommand
  :: String
  -> CLIArgs c
  -> (c -> IO ())
  -> IO ()
versionOrCommand v a run = either (const (showVersion v)) run (_command a)

-- | Parser for the common CLI options, wrapping the given command parser.
--
-- @since 0.1.0.0
argParser :: Parser c -> Parser (CLIArgs c)
argParser cmdParser =
  CLIArgs
    <$> optional
      ( option
          (maybeReader (`lookup` logLevels))
          ( short 'l'
              <> long "log-level"
              <> metavar "LEVEL"
              <> help ("Log level: " <> intercalate ", " (fmap fst logLevels))
          )
      )
    <*> switch (short 'd' <> long "debug" <> help "Enable debug output (overruled by -l)")
    <*> switch (short 'y' <> long "yes" <> help "Auto-accept all prompts")
    <*> switch (short 'n' <> long "no" <> help "Auto-decline all prompts (overrides -y/--yes)")
    <*> optional
      ( strOption
          (short 'c' <> long "config" <> metavar "PATH" <> help "Path to the configuration file")
      )
    <*> ( Left
            <$> flag' () (short 'v' <> long "version" <> help "display version information")
              <|> Right
            <$> cmdParser
        )
