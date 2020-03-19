module Fission.CLI.Command.Register.Types (Options(..)) where

import Fission.Prelude hiding (Options)

-- | Arguments, flags & switches for the `login` command
data Options = Options
  { local_auth :: Bool }
