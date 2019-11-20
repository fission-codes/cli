-- | Reading and writing local user config values
module Fission.CLI.Environment
  ( init
  , get
  , findLocalAuth
  , writeAuth
  , couldNotRead
  , removeConfigFile
  , getOrRetrievePeer
  ) where

import           Fission.Prelude
import           RIO.Directory
import           RIO.FilePath
import           Servant.API

import qualified System.Console.ANSI as ANSI
import           Data.List.NonEmpty  as NonEmpty hiding (init, (<|))

import           Fission.Web.Client.Peers as Peers
import qualified Fission.Web.Client.Types as Client

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error

import           Fission.CLI.Environment.Types
import           Fission.CLI.Environment.Partial.Types as Env
import qualified Fission.CLI.Environment.Partial as Env.Partial
import qualified Fission.CLI.Environment.Error as Error

import           Fission.Internal.Orphanage.BasicAuthData ()
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.IPFS.Peer  as IPFS.Peer
import qualified Fission.IPFS.Types as IPFS

-- | Initialize the Environment file
init :: MonadRIO cfg m
     => HasLogFunc        cfg
     => Has Client.Runner cfg
     => BasicAuthData
     -> m ()
init auth = do
  logDebug "Initializing config file"
  path <- globalEnv

  Peers.getPeers >>= \case
    Left err ->
      CLI.Error.put err "Peer retrieval failed"

    Right peers -> do
      let env = Env.Partial {
        maybeUserAuth = Just auth,
        maybePeers = Just (NonEmpty.fromList peers)
      }
      liftIO <| Env.Partial.write path env
      CLI.Success.putOk "Logged in"

-- | Gets hierarchical environment by recursed through file system
get :: MonadIO m => m (Either Error.Env Environment)
get = do 
  partial <- Env.Partial.get
  return <| Env.Partial.toFull partial

write :: MonadIO m => FilePath -> Environment -> m ()
write path env = Env.Partial.write path <| Env.Partial.fromFull env

-- | Locate current auth on the user's system
findLocalAuth :: MonadIO m => m (Either Error.Env FilePath)
findLocalAuth = do
  currDir <- getCurrentDirectory
  findRecurse (isJust . maybeUserAuth) currDir >>= \case
    Nothing -> return <| Left Error.EnvNotFound 
    Just path -> return <| Right path

findRecurse :: MonadIO m => (Env.Partial -> Bool) -> FilePath -> m (Maybe FilePath)
findRecurse f path = do 
  let filepath = path </> ".fission.yaml"
  partial <- Env.Partial.decode filepath
  case (f partial, path) of
    (True, _) -> return <| Just filepath
    (_, "/")  -> return Nothing
    _         -> findRecurse f <| takeDirectory path

-- | globalEnv environment in users home
globalEnv :: MonadIO m => m FilePath
globalEnv = do
  home <- getHomeDirectory
  return <| home </> ".fission.yaml"

writeAuth :: MonadRIO cfg m
          => BasicAuthData
          -> FilePath
          -> m ()
writeAuth auth path = do
  partial <- Env.Partial.decode path
  Env.Partial.write path <| partial { maybeUserAuth = Just auth }

-- | Create a could not read message for the terminal
couldNotRead :: MonadIO m => m ()
couldNotRead = do
  liftIO <| ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  UTF8.putText "🚫 Unable to read credentials. Try logging in with "

  liftIO <| ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  UTF8.putText "fission-cli login\n"

  liftIO <| ANSI.setSGR [ANSI.Reset]

-- | Removes the user's global config file
removeConfigFile :: MonadUnliftIO m => m (Either IOException ())
removeConfigFile = do
  path <- globalEnv
  try <| removeFile path

-- | Retrieves a Fission Peer from local config
--   If not found we retrive from the network and store
getOrRetrievePeer :: MonadRIO          cfg m
                  => HasLogFunc        cfg
                  => Has Client.Runner cfg
                  => Environment
                  -> m IPFS.Peer
getOrRetrievePeer config =
  case peers config of
    Just prs -> do
      logDebug "Retrieved Peer from .fission.yaml"
      return <| head prs

    Nothing ->
      Peers.getPeers >>= \case
        Left err -> do
          logError <| displayShow err
          logDebug "Unable to retrieve peers from the network, using default address"
          return <| IPFS.Peer.fission

        Right peers -> do
          logDebug "Retrieved Peer from API"
          path <- globalEnv
          write path <| config { peers = Just (NonEmpty.fromList peers) }
          return <| head <| NonEmpty.fromList peers
