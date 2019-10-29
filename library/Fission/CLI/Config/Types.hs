module Fission.CLI.Config.Types
  ( CommandM
  , Config (..)
  , fissionAPI
  , logFunc
  , UpConfig(..)
  , Uppable
  ) where

import RIO
import RIO.Process (ProcessContext, HasProcessContext (..))
import Servant.API

import Data.Has
import Control.Lens (makeLenses)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Lazy
import Options.Applicative as OA

import qualified Fission.Web.Client.Types as Client
import qualified Fission.IPFS.Types       as IPFS

-- | The action to attach to the command interface and description
type CommandM a = ExceptT a (Writer (Mod CommandFields a)) ()

-- | The configuration used for the CLI application
data Config = Config
  { _fissionAPI  :: !Client.Runner
  , _logFunc     :: !LogFunc
  , _processCtx  :: !ProcessContext
  , _ipfsPath    :: !IPFS.BinPath
  , _ipfsTimeout :: !IPFS.Timeout
  , _peers       :: Maybe (NonEmpty IPFS.Peer)
  , _userAuth    :: Maybe BasicAuthData
  }

makeLenses ''Config

instance HasLogFunc Config where
  logFuncL = logFunc

instance Has Client.Runner Config where
  hasLens = fissionAPI

instance HasProcessContext Config where
  processContextL = processCtx

instance Has IPFS.BinPath Config where
  hasLens = ipfsPath

instance Has IPFS.Timeout Config where
  hasLens = ipfsTimeout

instance Has (Maybe (NonEmpty IPFS.Peer)) Config where
  hasLens = peers

instance Has (Maybe BasicAuthData) Config where
  hasLens = userAuth


--- TODO: Move below into new files
-- Just took all of those constraints and named them Uppable. Please find a better name  or just use the Has :P
type Uppable cfg
  = ( HasLogFunc        cfg
    , HasProcessContext cfg
    , Has IPFS.Timeout  cfg
    , Has IPFS.BinPath  cfg
    , Has Client.Runner cfg
    , Has IPFS.Peer cfg
    )

-- | RENAME ME PLEASE! See note in Discord
data UpConfig = UpConfig
  { _fissionAPI'  :: !Client.Runner
  , _logFunc'     :: !LogFunc
  , _processCtx'  :: !ProcessContext
  , _ipfsPath'    :: !IPFS.BinPath
  , _ipfsTimeout' :: !IPFS.Timeout
  , _peer'        :: !IPFS.Peer
  }

makeLenses ''UpConfig

instance Has Client.Runner UpConfig where
  hasLens = fissionAPI'

instance HasLogFunc UpConfig where
  logFuncL = logFunc'

instance HasProcessContext UpConfig where
  processContextL = processCtx'

instance Has IPFS.BinPath UpConfig where
  hasLens = ipfsPath'

instance Has IPFS.Timeout UpConfig where
  hasLens = ipfsTimeout'

instance Has IPFS.Peer UpConfig where
  hasLens = peer'