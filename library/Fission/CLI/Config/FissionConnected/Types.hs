-- | Configuration required to run any CLI function that interacts with the Fission service
module Fission.CLI.Config.FissionConnected.Types
  ( FissionConnected (..)
  , HasFissionConnected
  , fissionAPI
  , logFunc
  , processCtx
  , ipfsPath
  , ipfsTimeout
  , peer
  , userAuth
  ) where

import RIO
import RIO.Process (ProcessContext, HasProcessContext (..))
import Servant.API

import Data.Has
import Control.Lens (makeLenses)

import qualified Fission.Web.Client.Types as Client
import qualified Fission.IPFS.Types       as IPFS

type HasFissionConnected cfg
  = ( HasLogFunc        cfg
    , HasProcessContext cfg
    , Has Client.Runner cfg
    , Has IPFS.Timeout  cfg
    , Has IPFS.BinPath  cfg
    , Has IPFS.Peer     cfg
    , Has BasicAuthData cfg
    )

data FissionConnected = FissionConnected
  { _fissionAPI  :: !Client.Runner
  , _logFunc     :: !LogFunc
  , _processCtx  :: !ProcessContext
  , _ipfsPath    :: !IPFS.BinPath
  , _ipfsTimeout :: !IPFS.Timeout
  , _peer        :: !IPFS.Peer
  , _userAuth    :: !BasicAuthData
  }

makeLenses ''FissionConnected

instance Has Client.Runner FissionConnected where
  hasLens = fissionAPI

instance HasLogFunc FissionConnected where
  logFuncL = logFunc

instance HasProcessContext FissionConnected where
  processContextL = processCtx

instance Has IPFS.BinPath FissionConnected where
  hasLens = ipfsPath

instance Has IPFS.Timeout FissionConnected where
  hasLens = ipfsTimeout

instance Has IPFS.Peer FissionConnected where
  hasLens = peer

instance Has BasicAuthData FissionConnected where
  hasLens = userAuth
