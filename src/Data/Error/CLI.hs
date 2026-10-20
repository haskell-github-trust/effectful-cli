module Data.Error.CLI (
  CLIError (..),
  displayCLIError,
) where

data CLIError
  = InputError String
  | NotFoundError String
  | NotImplemented String
  | UserCanceled (Maybe String)
  deriving (Eq, Show)

displayCLIError :: CLIError -> String
displayCLIError (InputError s) = "input error: " ++ s
displayCLIError (NotFoundError s) = "not found: " ++ s
displayCLIError (NotImplemented s) = "not implemented: " ++ s
displayCLIError (UserCanceled Nothing) = "user canceled"
displayCLIError (UserCanceled (Just s)) = "user canceled: " ++ s
