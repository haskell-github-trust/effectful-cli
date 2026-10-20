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

-- | @since 0.1.0.0
unboundEnumError :: String -> a
unboundEnumError name = error (printf "%s.Enum: succ invoked on maxBound" name)

-- | @since 0.1.0.0
fromParsec :: String -> Parsec Void String a -> ReadM a
fromParsec name p = eitherReader (first errorBundlePretty . runParser (p <* eof) name)

-- | @since 0.1.0.0
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

-- | @since 0.1.0.0
noArgs :: a -> Parser a
noArgs = pure

-- | @since 0.1.0.0
forceSwitch :: Parser Bool
forceSwitch = switch (short 'f' <> long "force" <> help "force unsafe behavior")

-- | @since 0.1.0.0
noopSwitch :: Parser Bool
noopSwitch = switch (short 'n' <> long "noop" <> help "do not perform any real action")

-- | @since 0.1.0.0
uuidArg :: Parser UUID
uuidArg = argOrOpt "uuid" 'u' "target uuid" (Right (maybeReader UUID.fromString)) []

-- | @since 0.1.0.0
mkCommand :: Command c e a => a -> Mod CommandFields a
mkCommand a = command (cmdname a) (info (parser a) (progDesc (doc a)))

-- | @since 0.1.0.0
mkCommands
  :: Bounded a => Command c e a => Command c e b => Enum a => (a -> b) -> Mod CommandFields b
mkCommands f = foldMap (mkCommand . f) (sortOn cmdname [minBound ..])

-- | @since 0.1.0.0
usage :: String -> String -> String -> Parser a -> ParserInfo a
usage name version hdr p =
  info
    (p <**> helper)
    (fullDesc <> progDesc hdr <> header (printf "%s - %s - version %s" name hdr version))
