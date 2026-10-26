{-# LANGUAGE OverloadedStrings #-}

import Data.CLI.Impl (fromParsec)
import Data.Error.CLI (CLIError (..), displayCLIError)
import Effectful (runEff)
import Effectful.CLI.Conc (batchConcurrentlyWithIndex)
import Effectful.CLI.IO (resolveIndex)
import Effectful.Concurrent (runConcurrent)
import Options.Applicative
import Test.Hspec
import Text.Megaparsec.Char (string)

isInputError :: Either CLIError a -> Bool
isInputError (Left (InputError _)) = True
isInputError _ = False

main :: IO ()
main = hspec $ do
  describe "batchConcurrentlyWithIndex" $ do
    let xs = "abcde"
        expected = zip [1 ..] xs
        run n = runEff . runConcurrent $ batchConcurrentlyWithIndex n (curry pure) xs

    it "pairs 1-based indices with elements" $
      run 10 `shouldReturn` expected

    it "works with a batch smaller than the list" $
      run 2 `shouldReturn` expected

  describe "resolveIndex" $ do
    let len = 4
    it "resolves positive bounds" $ do
      resolveIndex len 1 `shouldBe` Right 0
      resolveIndex len len `shouldBe` Right (len - 1)

    it "resolves negative bounds" $ do
      resolveIndex len (-1) `shouldBe` Right (len - 1)
      resolveIndex len (-len) `shouldBe` Right 0

    it "rejects out of bounds" $
      fmap (isInputError . resolveIndex len) [0, len + 1, -(len + 1)]
        `shouldBe` [True, True, True]

  describe "displayCLIError"
    $ it "renders every constructor"
    $ fmap
      displayCLIError
      [ InputError "a"
      , NotFoundError "b"
      , NotImplemented "c"
      , UserCanceled Nothing
      , UserCanceled (Just "d")
      ]
      `shouldBe` [ "input error: a"
                 , "not found: b"
                 , "not implemented: c"
                 , "user canceled"
                 , "user canceled: d"
                 ]

  describe "fromParsec" $ do
    let p = info (argument (fromParsec "word" (string "ok")) mempty) mempty
        parse = getParseResult . execParserPure defaultPrefs p

    it "accepts a valid input" $
      parse ["ok"] `shouldBe` Just "ok"

    it "rejects an invalid input" $
      parse ["ko"] `shouldBe` Nothing
