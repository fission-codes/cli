module Fission.CLI.Config.FissionConnected.Error.Types (Error (..)) where

import           RIO

data Error
  = NotFissionConnected
  | CannotConnect
  deriving (Eq, Show, Exception)
