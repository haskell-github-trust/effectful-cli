-- | Errors common to CLI applications.
module Data.Error.CLI (
  CLIError (..),
  displayCLIError,
) where

-- | Errors raised by CLI helpers.
--
-- @since 0.1.0.0
data CLIError
  = -- | Invalid user input.
    InputError String
  | -- | Requested item does not exist.
    NotFoundError String
  | -- | Feature not implemented yet.
    NotImplemented String
  | -- | User declined a confirmation, with an optional reason.
    UserCanceled (Maybe String)
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
