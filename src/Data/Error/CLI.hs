module Data.Error.CLI (
  CLIError (..),
  displayCLIError,
) where

-- | @since 0.1.0.0
data CLIError
  = InputError String
  | NotFoundError String
  | NotImplemented String
  | UserCanceled (Maybe String)
  deriving (Eq, Show)

-- | Render a 'CLIError' as a human-readable message.
--
-- @since 0.1.0.0
displayCLIError :: CLIError -> String
displayCLIError (InputError s) = "input error: " <> s
displayCLIError (NotFoundError s) = "not found: " <> s
displayCLIError (NotImplemented s) = "not implemented: " <> s
displayCLIError (UserCanceled Nothing) = "user canceled"
displayCLIError (UserCanceled (Just s)) = "user canceled: " <> s
