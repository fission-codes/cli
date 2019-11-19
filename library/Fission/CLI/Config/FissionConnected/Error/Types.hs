module Fission.CLI.Config.FissionConnected.Error.Types (Error (..)) where

import           Fission.Prelude

data Error
  = NotFissionConnected
  | CannotConnect
  deriving (Eq, Show, Exception)
