module Effectful.CLI (
  CLIEffects,
) where

import Data.Error.CLI (CLIError)
import Effectful (IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.HttpClient (HttpClient)
import Effectful.Logger (Logger)
import Effectful.Reader.Static (Reader)

type CLIEffects c e es =
  ( IOE :> es
  , Reader c :> es
  , Error CLIError :> es
  , Error e :> es
  , Logger :> es
  , HttpClient :> es
  )
