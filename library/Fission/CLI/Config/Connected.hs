-- | Guards to ensure we have the appropriate data available to run a protected action
module Fission.CLI.Config.Connected
  ( runConnected
  , runConnected'
  , liftConfig
  , module Fission.CLI.Config.Connected.Types
  ) where

import           Fission.Prelude

import           Network.IPFS

import           Fission.Web.Client

import           Fission.CLI.Config.Base

import           Fission.CLI.Config.Connected.Types
import           Fission.CLI.Config.Connected.Error.Types

import           Fission.CLI.Environment.Types as Environment
import qualified Fission.CLI.Environment       as Environment
import qualified Fission.CLI.IPFS.Connect      as Connect

-- | Ensure we have a local config file with the appropriate data
--
-- Takes a @Connected@-dependant action, and lifts it into an environment that
-- contains a superset of the environment
runConnected ::
  MonadIO m
  => BaseConfig
  -> FissionConnected a
  -> m (Either Error a)
runConnected cfg actions =
  runBase cfg <| liftConfig cfg >>= \case
    Right cfg' -> do
      result <- runConnected' cfg' actions
      return <| Right result

    Left err -> return <| Left err 

runConnected' ::
  MonadIO m
  => ConnectedConfig
  -> FissionConnected a
  -> m a
runConnected' cfg actions =
  actions
    |> unwrapFissionConnected
    |> runRIO cfg

liftConfig ::
  ( MonadUnliftIO         m
  , MonadLocalIPFS        m
  , MonadWebClient        m
  , MonadLogger           m
  )
  => BaseConfig
  -> m (Either Error ConnectedConfig)
liftConfig BaseConfig {..} =
  -- Get our stored user config
  Environment.get >>= \case
    Right config ->
      Environment.getOrRetrievePeer config >>= \case
        Just peer -> do
          let userAuth     = Environment.userAuth config
          let ignoredFiles = Environment.ignored config

          -- Connect the local IPFS node to the Fission network
          Connect.swarmConnectWithRetry peer 1 >>= \case
            Right _ -> do
              -- All setup and ready to run!
              return <| Right ConnectedConfig {..}

            Left err -> do
              -- We were unable to connect!
              logError <| displayShow err
              Connect.couldNotSwarmConnect
              return <| Left CannotConnect

        Nothing -> do
          logErrorN "Could not locate the Fission IPFS network"
          return <| Left PeersNotFound

    Left err -> do
      -- We were unable to read the users config
      logDebug <| displayShow err
      Environment.couldNotRead
      return <| Left NotFissionConnected
