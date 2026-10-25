{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Terminal output, prompts and confirmations.
module Effectful.CLI.IO (
  printOutput,
  printOutputLn,
  printError,
  printLines,
  prompt,
  prompt',
  askForConfirm,
  withConfirm,
  autoOpen,
  chooseItem,
  resolveIndex,
) where

import Control.Monad (unless, void, when)
import Data.CLI.Class (CLIConfig (..))
import Data.Error.CLI (CLIError (..))
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Process (Process, callProcess)
import Effectful.Reader.Static (Reader, ask)
import System.IO (hFlush, hPutStr, stderr)
import Text.Layout.Table (columnHeaderTableS, def, rowsG, tableString, titlesH, unicodeS)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, runParser, (<|>))
import Text.Megaparsec.Char (string')
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Printf (printf)

-- | Print to stdout without a trailing newline.
--
-- @since 0.1.0.0
printOutput :: IOE :> es => String -> Eff es ()
printOutput = liftIO . putStr

-- | Print to stdout with a trailing newline.
--
-- @since 0.1.0.0
printOutputLn :: IOE :> es => String -> Eff es ()
printOutputLn = liftIO . putStrLn

-- | Print to stderr without a trailing newline.
--
-- @since 0.1.0.0
printError :: IOE :> es => String -> Eff es ()
printError = liftIO . hPutStr stderr

-- | Print lines to stdout, each followed by a newline.
--
-- @since 0.1.0.0
printLines :: IOE :> es => [String] -> Eff es ()
printLines = printOutput . unlines

-- | Read a line from stdin after printing an optional prompt to stderr.
--
-- @since 0.1.0.0
prompt :: IOE :> es => Maybe String -> Eff es String
prompt p = liftIO $ do
  hPutStr stderr (fromMaybe "" p <> "> ")
  hFlush stderr
  getLine

-- | Prompt and parse the answer, throwing 'InputError' on parse failure.
--
-- @since 0.1.0.0
prompt'
  :: Error CLIError :> es => IOE :> es => Maybe String -> Parsec Void String a -> Eff es a
prompt' p parser =
  prompt p
    >>= either (throwError . InputError . errorBundlePretty) pure . runParser parser "prompt"

-- | Ask for confirmation; @y@, @yes@ or an empty answer mean yes.
--
-- @since 0.1.0.0
askForConfirm :: Error CLIError :> es => IOE :> es => Eff es Bool
askForConfirm = prompt' (Just "proceed ?") yesOrNo
 where
  yes = void (string' "y" <|> string' "yes") <|> eof
  yesOrNo = (yes $> True) <|> pure False

-- | Run the action after user confirmation, honoring the config's auto-accept and
-- auto-decline flags; throws 'UserCanceled' when declined.
--
-- Ambiguous in @c@: call as @withConfirm @MyConfig@.
--
-- @since 0.1.0.0
withConfirm
  :: forall c es a
   . CLIConfig c => Error CLIError :> es => IOE :> es => Reader c :> es => Eff es a -> Eff es a
withConfirm action = do
  c <- ask @c
  when (autoDecline c) $ throwError (UserCanceled (Just "auto-declined"))
  unless (autoAccept c) $ do
    ok <- askForConfirm
    unless ok $ throwError (UserCanceled Nothing)
  action

-- | Open a file or URL with @xdg-open@.
--
-- @since 0.1.0.0
autoOpen :: Process :> es => String -> Eff es ()
autoOpen arg = callProcess "xdg-open" [arg]

-- | Let the user pick one item from a table; a single item is returned without prompting, none throws 'NotFoundError'.
--
-- @since 0.1.0.0
chooseItem
  :: Error CLIError :> es
  => IOE :> es
  => String -> [String] -> (a -> [String]) -> [a] -> Eff es a
chooseItem name _ _ [] = throwError (NotFoundError ("no item found for " <> name))
chooseItem _ _ _ [x] = pure x
chooseItem name headers row xs = do
  let hs = "#" : headers
      rows = zipWith (\i x -> show i : row x) [1 :: Int ..] xs
  printOutputLn
    (tableString (columnHeaderTableS (fmap (const def) hs) unicodeS (titlesH hs) [rowsG rows]))
  i <- prompt' (Just name) (signed (pure ()) decimal)
  either throwError (pure . (xs !!)) (resolveIndex (length xs) i)

-- | Turn a 1-based (or negative, from the end) user index into a 0-based one.
--
-- @since 0.1.0.0
resolveIndex :: Int -> Int -> Either CLIError Int
resolveIndex len i
  | i >= 1 && i <= len = Right (i - 1)
  | i <= -1 && i >= -len = Right (len + i)
  | otherwise = Left (InputError (printf "index %d out of bounds" i))
