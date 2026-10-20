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

data CLIArgs c = CLIArgs
  { _logLevel :: Maybe LogLevel
  , _debug :: Bool
  , _autoAccept :: Bool
  , _autoDecline :: Bool
  , _config :: Maybe FilePath
  , _command :: Either () c
  }

logLevels :: [(String, LogLevel)]
logLevels =
  [("debug", LevelDebug), ("info", LevelInfo), ("warn", LevelWarn), ("error", LevelError)]

getLogLevel :: CLIArgs c -> LogLevel
getLogLevel a = fromMaybe (if _debug a then LevelDebug else LevelInfo) (_logLevel a)

showVersion :: String -> IO ()
showVersion = putStrLn

versionOrCommand :: String -> CLIArgs c -> (c -> IO ()) -> IO ()
versionOrCommand v a run = either (const (showVersion v)) run (_command a)

argParser :: Parser c -> Parser (CLIArgs c)
argParser cmdParser =
  CLIArgs
    <$> optional
      ( option
          (maybeReader (`lookup` logLevels))
          ( short 'l'
              <> long "log-level"
              <> metavar "LEVEL"
              <> help ("Log level: " ++ intercalate ", " (fmap fst logLevels))
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
