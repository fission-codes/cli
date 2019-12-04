-- | Guards to ensure we have the appropriate data available to run a protected action
module Fission.CLI.Config.FissionConnected
  ( ensure
  , module Fission.CLI.Config.FissionConnected.Types
  ) where

import           Fission.Prelude
import           RIO.Process (ProcessContext, HasProcessContext (..))

import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Web.Client   as Client

import qualified Fission.Config as Config
import           Fission.CLI.Config.FissionConnected.Types
import           Fission.CLI.Config.FissionConnected.Error.Types

import           Fission.CLI.Environment.Types as Environment
import qualified Fission.CLI.Environment       as Environment
import qualified Fission.CLI.IPFS.Connect      as Connect

-- | Ensure we have a local config file with the appropriate data
--
-- Takes a @FissionConnected@-dependant action, and lifts it into an environment that
-- contains a superset of the environment
ensure
  :: ( MonadRIO          cfg m
     , HasLogFunc        cfg
     , HasProcessContext cfg
     , Has IPFS.BinPath  cfg
     , Has IPFS.Timeout  cfg
     , Has Client.Runner cfg
     )
  => RIO FissionConnected a
  -> m (Either Error a)
ensure action = do
  _logFunc     :: LogFunc        <- view logFuncL
  _processCtx  :: ProcessContext <- view processContextL
  _fissionAPI  :: Client.Runner  <- Config.get
  _ipfsPath    :: IPFS.BinPath   <- Config.get
  _ipfsTimeout :: IPFS.Timeout   <- Config.get

  -- Get our stored user config
  Environment.get >>= \case
    Right config -> do
      Environment.getOrRetrievePeer config >>= \case
        Just _peer -> do
          let _userAuth     = Environment.userAuth config
          let _ignoredFiles = Environment.ignored config

          -- Connect the local IPFS node to the Fission network
          Connect.swarmConnectWithRetry _peer 1 >>= \case
            Right _ -> do
              -- All setup and ready to run!
              result <- liftIO <| runRIO FissionConnected {..} action
              return <| Right result

            Left err -> do
              -- We were unable to connect!
              logError <| displayShow err
              Connect.couldNotSwarmConnect
              return <| Left CannotConnect

        Nothing -> do
          logError "Could not locate the Fission IPFS network"
          return <| Left PeersNotFound

    Left err -> do
      -- We were unable to read the users config
      logDebug <| displayShow err
      Environment.couldNotRead
      return <| Left NotFissionConnected
