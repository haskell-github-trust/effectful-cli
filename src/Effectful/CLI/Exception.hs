-- | Bubble up IO exceptions as application errors.
module Effectful.CLI.Exception (
  rethrowIO,
) where

import Control.Exception (IOException)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (handle)

-- | Catch 'IOException's raised by an action and rethrow them as @e@ via the given constructor.
rethrowIO :: Error e :> es => Show e => (IOException -> e) -> Eff es a -> Eff es a
rethrowIO f = handle (throwError . f)
