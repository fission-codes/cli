module Fission.CLI.IPFS.Error.Types (Err (..)) where

import           RIO

data Err = UnableToConnect
  deriving (Show, Exception)
