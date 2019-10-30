module Fission.CLI.Config.Types.LoggedIn where

import RIO
import RIO.Process (ProcessContext, HasProcessContext (..))
import Servant.API

import Data.Has
import Control.Lens (makeLenses)

import qualified Fission.Web.Client.Types as Client
import qualified Fission.IPFS.Types       as IPFS

type HasLoggedIn cfg
  = ( HasLogFunc        cfg
    , HasProcessContext cfg
    , Has Client.Runner cfg
    , Has IPFS.Timeout  cfg
    , Has IPFS.BinPath  cfg
    , Has IPFS.Peer     cfg
    , Has BasicAuthData cfg
    )


data LoggedIn = LoggedIn
  { _fissionAPI'  :: !Client.Runner
  , _logFunc'     :: !LogFunc
  , _processCtx'  :: !ProcessContext
  , _ipfsPath'    :: !IPFS.BinPath
  , _ipfsTimeout' :: !IPFS.Timeout
  , _peer'        :: !IPFS.Peer
  , _userAuth'    :: !BasicAuthData
  }

makeLenses ''LoggedIn

instance Has Client.Runner LoggedIn where
  hasLens = fissionAPI'

instance HasLogFunc LoggedIn where
  logFuncL = logFunc'

instance HasProcessContext LoggedIn where
  processContextL = processCtx'

instance Has IPFS.BinPath LoggedIn where
  hasLens = ipfsPath'

instance Has IPFS.Timeout LoggedIn where
  hasLens = ipfsTimeout'

instance Has IPFS.Peer LoggedIn where
  hasLens = peer'

instance Has BasicAuthData LoggedIn where
  hasLens = userAuth'