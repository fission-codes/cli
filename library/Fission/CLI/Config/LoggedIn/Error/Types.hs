module Fission.CLI.Config.LoggedIn.Error.Types (Error (..)) where

import           Fission.Prelude

data Error = NotLoggedIn
  deriving (Eq, Show, Exception)
