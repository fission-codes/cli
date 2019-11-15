-- | Guards to ensure we have the appropriate data available to run a protected action
module Fission.CLI.Config.LoggedIn
  ( ensure
  , module Fission.CLI.Config.LoggedIn.Types
  ) where

import           RIO
import           RIO.Process (ProcessContext, HasProcessContext (..))

import           Data.Has

import           Fission.Internal.Constraint

import qualified Fission.IPFS.Types   as IPFS
import qualified Fission.Web.Client   as Client

import qualified Fission.Config as Config
import           Fission.CLI.Config.LoggedIn.Types
import           Fission.CLI.Config.LoggedIn.Error.Types

import           Fission.CLI.Environment.Types as Environment
import qualified Fission.CLI.Environment       as Environment
import qualified Fission.CLI.IPFS.Connect      as Connect

-- | Ensure we have a local config file with the appropriate data
--
-- Takes a @LoggedIn@-dependant action, and lifts it into an environment that
-- contains a superset of the environment
ensure
  :: ( MonadRIO          cfg m
     , MonadUnliftIO         m
     , HasLogFunc        cfg
     , HasProcessContext cfg
     , Has IPFS.BinPath  cfg
     , Has IPFS.Timeout  cfg
     , Has Client.Runner cfg
     )
  => RIO LoggedIn a
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
      _peer         <- Environment.getOrRetrievePeer config
      let _userAuth = Environment.userAuth config

      -- Connect the local IPFS node to the Fission network
      Connect.swarmConnectWithRetry _peer 1 >>= \case
        Right _ -> do
          -- All setup and ready to run!
          result <- liftIO $ runRIO LoggedIn {..} action
          Right <$> return result

        Left err -> do
          -- We were unable to connect!
          logError $ displayShow err
          Connect.couldNotSwarmConnect
          return $ Left CannotConnect

    Left err -> do
      -- We were unable to read the users config
      logDebug $ displayShow err
      Environment.couldNotRead
      return $ Left NotLoggedIn