-- | Reading and writing local user config values
module Fission.CLI.Environment
  ( init
  , get
  , overwriteLocalAuth
  , couldNotRead
  , removeConfigFile
  , getOrRetrievePeer
  ) where

import           RIO           hiding (set)
import           RIO.Directory
import           RIO.FilePath
import           Servant.API

import qualified System.Console.ANSI as ANSI

import           Data.Has
import           Data.List.NonEmpty as NonEmpty hiding (init)

import           Fission.Internal.Constraint

import           Fission.Web.Client.Peers as Peers
import qualified Fission.Web.Client.Types as Client

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error

import           Fission.CLI.Environment.Types
import           Fission.CLI.Environment.Partial.Types as Environment
import qualified Fission.CLI.Environment.Partial as Partial
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
      let env = Environment.Partial {
        maybeUserAuth = Just auth,
        maybePeers = Just (NonEmpty.fromList peers)
      }
      liftIO $ Partial.write env path
      CLI.Success.putOk "Logged in"

-- | Gets hierarchical environment by recursed through file system
get :: MonadIO m => m (Either Error.Env Environment)
get = do 
  partial <- Partial.get
  return $ Partial.toFull partial

-- | Locate current auth on the user's system
findLocalAuth :: MonadIO m => m (Maybe FilePath)
findLocalAuth = do
  currDir <- getCurrentDirectory
  findRecurse (isJust . maybeUserAuth) currDir

findRecurse :: MonadIO m => (Environment.Partial -> Bool) -> FilePath -> m (Maybe FilePath)
findRecurse fn path = do 
  let filepath = path </> ".fission.yaml"
  partial <- Partial.decode filepath
  let exists = fn partial
  if exists
    then return $ Just filepath
    else case path of
      "/" -> return Nothing
      _   -> findRecurse fn $ takeDirectory path

-- | globalEnv environment in users home
globalEnv :: MonadIO m => m FilePath
globalEnv = do
  home <- getHomeDirectory
  return $ home </> ".fission.yaml"

overwriteLocalAuth :: MonadRIO cfg m
      => BasicAuthData
      -> m (Either SomeException Bool)
overwriteLocalAuth auth = 
  findLocalAuth >>= \case
    Nothing -> return . Left $ toException Error.EnvNotFound 
    Just path -> do
      partial <- Partial.decode path
      let updated = Partial.updateAuth partial auth
      Partial.write updated path
      return $ Right True

-- | Create a could not read message for the terminal
couldNotRead :: MonadIO m => m ()
couldNotRead = do
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  UTF8.putText "🚫 Unable to read credentials. Try logging in with "

  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  UTF8.putText "fission-cli login\n"

  liftIO $ ANSI.setSGR [ANSI.Reset]

-- | Removes the user's global config file
removeConfigFile :: MonadUnliftIO m => m (Either IOException ())
removeConfigFile = do
  path <- globalEnv
  try $ removeFile path

-- | Retrieves a Fission Peer from local config
--   If not found we retrive from the network and store
getOrRetrievePeer :: MonadRIO          cfg m
                  => MonadUnliftIO         m
                  => HasLogFunc        cfg
                  => Has Client.Runner cfg
                  => Environment
                  -> m IPFS.Peer
getOrRetrievePeer config =
  case peers config of
    Just prs -> do
      logDebug "Retrieved Peer from .fission.yaml"
      return $ head prs

    Nothing ->
      Peers.getPeers >>= \case
        Left err -> do
          logError $ displayShow err
          logDebug "Unable to retrieve peers from the network, using default address"
          return $ IPFS.Peer.fission

        Right peers -> do
          logDebug "Retrieved Peer from API"
          let updated = Partial.updatePeers (Partial.fromFull config) peers
          Partial.write updated =<< globalEnv
          return $ head $ NonEmpty.fromList peers
