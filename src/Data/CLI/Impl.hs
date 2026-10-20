module Data.CLI.Impl (
  unboundEnumError,
  fromParsec,
  argOrOpt,
  noArgs,
  forceSwitch,
  noopSwitch,
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

unboundEnumError :: String -> a
unboundEnumError name = error (printf "%s.Enum: succ invoked on maxBound" name)

fromParsec :: String -> Parsec Void String a -> ReadM a
fromParsec name p = eitherReader (first errorBundlePretty . runParser (p <* eof) name)

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

noArgs :: a -> Parser a
noArgs = pure

forceSwitch :: Parser Bool
forceSwitch = switch (short 'f' <> long "force" <> help "force unsafe behavior")

noopSwitch :: Parser Bool
noopSwitch = switch (short 'n' <> long "noop" <> help "do not perform any real action")

uuidArg :: Parser UUID
uuidArg = argOrOpt "uuid" 'u' "target uuid" (Right (maybeReader UUID.fromString)) []

mkCommand :: Command c e a => a -> Mod CommandFields a
mkCommand a = command (cmdname a) (info (parser a) (progDesc (doc a)))

mkCommands
  :: Bounded a => Command c e a => Command c e b => Enum a => (a -> b) -> Mod CommandFields b
mkCommands f = foldMap (mkCommand . f) (sortOn cmdname [minBound ..])

usage :: String -> String -> String -> Parser a -> ParserInfo a
usage name version hdr p =
  info
    (p <**> helper)
    (fullDesc <> progDesc hdr <> header (printf "%s - %s - version %s" name hdr version))
