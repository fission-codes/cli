-- | Guards to ensure we have the appropriate peer data available to run an action
module Fission.CLI.Command.Guard.Peers where

import           RIO
import           RIO.Process (ProcessContext, HasProcessContext (..))

import           Data.Has
import           Data.List.NonEmpty

import           Fission.Internal.Constraint
import           Fission.Internal.Exception

import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Web.Client   as Client

import           Fission.CLI.Config.Types
import qualified Fission.Config as Config
import qualified Fission.CLI.Auth as Auth
import           Fission.CLI.Environment.Types

ensurePeers
  :: ( MonadRIO          cfg  m
  , HasLogFunc        cfg
  , HasProcessContext cfg
  , Has IPFS.BinPath  cfg
  , Has IPFS.Timeout  cfg
  , Has Client.Runner cfg
  )
  => RIO UpConfig a
  -> m a
ensurePeers handler = do
  _logFunc'     :: LogFunc        <- view logFuncL
  _processCtx'  :: ProcessContext <- view processContextL
  _fissionAPI'  :: Client.Runner  <- Config.get
  _ipfsPath'    :: IPFS.BinPath   <- Config.get
  _ipfsTimeout' :: IPFS.Timeout   <- Config.get

  Auth.get >>= \case
    Right config -> do
      let _userAuth' = (userAuth config)
          _peer'     = head $ (peers config)

      localRIO UpConfig {..} handler

    Left err -> do
      logError $ displayShow err
      Auth.couldNotRead
      return undefined

localRIO :: MonadRIO oldCfg m => newCfg -> RIO newCfg a -> m a
localRIO newCfg action = liftIO $ runRIO newCfg action
