-- | Guards to ensure we have the appropriate data available to run a protected action
module Fission.CLI.Config.Guard where

import           RIO
import           RIO.Process (ProcessContext, HasProcessContext (..))

import           Data.Has

import           Fission.Internal.Constraint

import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Web.Client   as Client

import qualified Fission.Config as Config

import           Fission.CLI.Environment.Types
import qualified Fission.CLI.Environment as Environment
import           Fission.CLI.Config.Types
import qualified Fission.CLI.IPFS.Connect as Connect

-- | Ensure we have a local config file with the appropriate data
ensureLocalConfig
  :: ( MonadRIO          cfg  m
  , MonadUnliftIO         m
  , HasLogFunc        cfg
  , HasProcessContext cfg
  , Has IPFS.BinPath  cfg
  , Has IPFS.Timeout  cfg
  , Has Client.Runner cfg
  )
  => RIO LoggedIn a
  -> m a
ensureLocalConfig handler = do
  _logFunc'     :: LogFunc        <- view logFuncL
  _processCtx'  :: ProcessContext <- view processContextL
  _fissionAPI'  :: Client.Runner  <- Config.get
  _ipfsPath'    :: IPFS.BinPath   <- Config.get
  _ipfsTimeout' :: IPFS.Timeout   <- Config.get

  -- Get our stored user config
  Environment.get >>= \case
    Right config -> do
      _peer'         <- Environment.getOrRetrievePeer config
      let _userAuth' = (userAuth config)

      -- Connect the local IPFS node to the Fission network
      Connect.swarmConnectWithRetry _peer' 1 >>= \case
        Right _ ->
          -- All setup and ready to run!
          localRIO LoggedIn {..} handler

        Left err -> do
          -- We were unable to connect!
          logError $ displayShow err
          Connect.couldNotSwarmConnect
          return undefined

    Left err -> do
      -- We were unable to read the users config
      logError $ displayShow err
      Environment.couldNotRead
      return undefined

localRIO :: MonadRIO oldCfg m => newCfg -> RIO newCfg a -> m a
localRIO newCfg action = liftIO $ runRIO newCfg action
