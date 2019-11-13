-- | General configuration required to run any CLI function
module Fission.CLI.Config.Base.Types
  ( BaseConfig (..)
  , logFunc
  , fissionAPI
  , processCtx
  , ipfsPath
  , ipfsTimeout
  ) where

import RIO
import RIO.Process (ProcessContext, HasProcessContext (..))

import Data.Has
import Control.Lens (makeLenses)

import qualified Fission.Web.Client.Types as Client
import qualified Fission.IPFS.Types       as IPFS

-- | The configuration used for the CLI application
data BaseConfig = BaseConfig
  { _fissionAPI  :: !Client.Runner
  , _logFunc     :: !LogFunc
  , _processCtx  :: !ProcessContext
  , _ipfsPath    :: !IPFS.BinPath
  , _ipfsTimeout :: !IPFS.Timeout
  }

makeLenses ''BaseConfig

instance HasLogFunc BaseConfig where
  logFuncL = logFunc

instance Has Client.Runner BaseConfig where
  hasLens = fissionAPI

instance HasProcessContext BaseConfig where
  processContextL = processCtx

instance Has IPFS.BinPath BaseConfig where
  hasLens = ipfsPath

instance Has IPFS.Timeout BaseConfig where
  hasLens = ipfsTimeout
