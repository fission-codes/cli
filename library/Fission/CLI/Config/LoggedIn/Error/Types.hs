module Fission.CLI.Config.LoggedIn.Error.Types (Error (..)) where

import           RIO

data Error = NotLoggedIn
  deriving (Eq, Show, Exception)
