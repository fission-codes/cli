module Fission.CLI.IPFS.Error.Types where
import RIO

data Err = UnableToConnect
  deriving (Show, Exception)
