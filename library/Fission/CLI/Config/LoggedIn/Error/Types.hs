module Fission.CLI.Config.LoggedIn.Error.Types (Error (..)) where

import           RIO

data Error
  = NotLoggedIn
  | CannotConnect
  deriving (Eq, Show, Exception)
