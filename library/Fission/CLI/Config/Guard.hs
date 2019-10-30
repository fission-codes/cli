-- | Guards to ensure we have the appropriate data available to run a protected action
module Fission.CLI.Config.Guard where

import           RIO
import           RIO.Process (ProcessContext, HasProcessContext (..))

import           Data.Has
import           Data.List.NonEmpty

import           Fission.Internal.Constraint

import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.IPFS.Peer    as IPFS.Peer
import qualified Fission.Web.Client   as Client

import           Fission.CLI.Config.Types
import qualified Fission.Config as Config
import qualified Fission.CLI.Environment as Environment
import           Fission.CLI.Environment.Types
import qualified Fission.CLI.Display.Wait    as CLI.Wait

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

  Environment.get >>= \case
    Right config -> do
      let _userAuth' = (userAuth config)
          _peer'     = head $ (peers config)

      CLI.Wait.waitFor "Connecting to Fission nodes..."
        $ IPFS.Peer.connect _peer' >>= \case
          Right _ ->
            localRIO LoggedIn {..} handler

          Left err -> do
            logError $ displayShow err
            Environment.couldNotSwarmConnect
            return undefined

    Left err -> do
      logError $ displayShow err
      Environment.couldNotRead
      return undefined

localRIO :: MonadRIO oldCfg m => newCfg -> RIO newCfg a -> m a
localRIO newCfg action = liftIO $ runRIO newCfg action
