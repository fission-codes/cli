module Fission.CLI.Command.Login.Types (Options(..)) where

import Fission.Prelude hiding (Options)

-- | Arguments, flags & switches for the `login` command
data Options = Options{ username2 :: String }
