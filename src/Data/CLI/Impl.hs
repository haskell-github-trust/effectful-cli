-- | Helpers for building optparse-applicative parsers and subcommands.
module Data.CLI.Impl (
  unboundEnumError,
  fromParsec,
  argOrOpt,
  noArgs,
  forceSwitch,
  noopSwitch,
  reversibleSwitch,
  uuidArg,
  mkCommand,
  mkCommands,
  usage,
) where

import Data.Bifunctor (first)
import Data.CLI.Class (Command (..))
import Data.Char (toUpper)
import Data.List (sortOn)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Void (Void)
import Options.Applicative
import Text.Megaparsec (Parsec, eof, errorBundlePretty, runParser)
import Text.Printf (printf)

-- | Error for 'succ' called on 'maxBound' of the named enum.
--
-- @since 0.1.0.0
unboundEnumError :: String -> a
unboundEnumError name = error (printf "%s.Enum: succ invoked on maxBound" name)

-- | Turn a megaparsec parser into a 'ReadM', requiring the whole input to be consumed.
--
-- @since 0.1.0.0
fromParsec :: String -> Parsec Void String a -> ReadM a
fromParsec name p = eitherReader (first errorBundlePretty . runParser (p <* eof) name)

-- | Accept a value either as a positional argument or as a @-c@/@--name@ option.
--
-- @since 0.1.0.0
argOrOpt
  :: String
  -> Char
  -> String
  -> Either (Parsec Void String a) (ReadM a)
  -> [String]
  -> Parser a
argOrOpt name c h p cs =
  argument r (metavar var <> completeWith cs)
    <|> option r (short c <> long name <> help h <> metavar var <> completeWith cs)
 where
  var = fmap toUpper name
  r = either (fromParsec name) id p

-- | Parser for a command that takes no arguments.
--
-- @since 0.1.0.0
noArgs :: a -> Parser a
noArgs = pure

-- | @-f@/@--force@ switch.
--
-- @since 0.1.0.0
forceSwitch :: Parser Bool
forceSwitch = switch (short 'f' <> long "force" <> help "force unsafe behavior")

-- | @-n@/@--noop@ switch for dry runs.
--
-- @since 0.1.0.0
noopSwitch :: Parser Bool
noopSwitch = switch (short 'n' <> long "noop" <> help "do not perform any real action")

-- | @--name@/@--no-name@ switch pair; the last occurrence wins, otherwise the default.
--
-- @since 0.2.2.0
reversibleSwitch :: Bool -> String -> String -> Parser Bool
reversibleSwitch def name h =
  last
    <$> some (flag' True (long name <> help h) <|> flag' False (long ("no-" <> name) <> hidden))
      <|> pure def

-- | UUID given as a positional argument or @-u@/@--uuid@.
--
-- @since 0.1.0.0
uuidArg :: Parser UUID
uuidArg = argOrOpt "uuid" 'u' "target uuid" (Right (maybeReader UUID.fromString)) []

-- | Build a subcommand from a 'Command' instance.
--
-- @since 0.1.0.0
mkCommand :: Command c e a => a -> Mod CommandFields a
mkCommand a = command (cmdname a) (info (parser a) (progDesc (doc a)))

-- | Build subcommands for every value of an enum, sorted by name and wrapped with the given constructor.
--
-- @since 0.1.0.0
mkCommands
  :: Bounded a => Command c e a => Command c e b => Enum a => (a -> b) -> Mod CommandFields b
mkCommands f = foldMap (mkCommand . f) (sortOn cmdname [minBound ..])

-- | Top-level t'ParserInfo' with @--help@ and a header showing name, description and version.
--
-- @since 0.1.0.0
usage :: String -> String -> String -> Parser a -> ParserInfo a
usage name version hdr p =
  info
    (p <**> helper)
    (fullDesc <> progDesc hdr <> header (printf "%s - %s - version %s" name hdr version))
