-- | Effect stack available to command runners.
module Effectful.CLI (
  CLIEffects,
) where

import Data.Error.CLI (CLIError)
import Effectful (IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.HttpClient (HttpClient)
import Effectful.Logger (Logger)
import Effectful.Reader.Static (Reader)

-- | Effects available to a command runner, with config @c@ and application error @e@.
--
-- @since 0.1.0.0
type CLIEffects c e es =
  ( IOE :> es
  , Reader c :> es
  , Error CLIError :> es
  , Error e :> es
  , Logger :> es
  , HttpClient :> es
  )
