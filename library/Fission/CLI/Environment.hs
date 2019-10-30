-- | Reading and writing local user config values
module Fission.CLI.Environment where

import           RIO           hiding (set)
import           RIO.Directory
import           RIO.File
import           RIO.FilePath
import           Servant.API

import qualified System.Console.ANSI as ANSI

import           Data.Has
import qualified Data.Yaml as YAML
import           Data.List.NonEmpty as NonEmpty

import qualified Fission.Config as Config
import           Fission.Internal.Constraint

import           Fission.Web.Client.Peers as Peers
import qualified Fission.Web.Client.Types as Client

import qualified Fission.CLI.Display.Cursor  as Cursor
import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait

import           Fission.CLI.Environment.Types

import           Fission.Internal.Orphanage.BasicAuthData ()
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.IPFS.Peer    as IPFS.Peer
import qualified Fission.IPFS.Types    as IPFS

-- | Initialize the Config file
init :: MonadRIO cfg m
          => HasLogFunc        cfg
          => Has Client.Runner cfg
          => BasicAuthData
          -> m ()
init auth = do
  Client.Runner run <- Config.get

  peersResponse <- liftIO
        $ Cursor.withHidden
        $ CLI.Wait.waitFor "Retrieving Fission Peer List..."
        $ run
        $ Peers.get

  case peersResponse of
    Left err ->
      CLI.Error.put err "Peer retrieval failed"

    Right peers -> do
      liftIO $ write auth peers
      CLI.Success.putOk "Logged in"

swarmConnectWithRetry peer = CLI.Wait.waitFor "Connecting to Fission nodes..." $ swarmConnectWithRetry' peer 1

swarmConnectWithRetry' :: MonadRIO cfg m
          => HasLogFunc        cfg
          => Has Client.Runner cfg
          => IPFS.Peer
          -> Int
          -> m (Either SomeException ())
swarmConnectWithRetry' peer (-1) = return $ Left "Failure"
swarmConnectWithRetry' peer retries = do
  Client.Runner run <- Config.get

  IPFS.Peer.connect peer >>= \case
    Right _ ->
      return $ Right ()

    Left _err -> do
      peersResponse <- liftIO
          $ Cursor.withHidden
          $ CLI.Wait.waitFor "Retrieving Fission Peer List..."
          $ run
          $ Peers.get

      case peersResponse of
        Left err ->
          return $ Left "Failure"

        Right peers -> do
          let peer = head $ NonEmpty.fromList peers
          return $ swarmConnectWithRetry' peer (retries - 1)

-- | Retrieve auth from the user's system
get :: MonadIO m => m (Either YAML.ParseException Environment)
get = liftIO . YAML.decodeFileEither =<< cachePath

-- | Write user's auth to a local on-system path
write :: MonadUnliftIO m => BasicAuthData -> [IPFS.Peer] -> m ()
write auth peers = do
  path <- cachePath
  let configFileContent = Environment {
                            peers = NonEmpty.fromList peers
                          , userAuth = auth
                          }
  writeBinaryFileDurable path $ YAML.encode $ configFileContent

-- | Absolute path of the auth cache on disk
cachePath :: MonadIO m => m FilePath
cachePath = do
  home <- getHomeDirectory
  return $ home </> ".fission.yaml"

-- | Create a could not read message for the terminal
couldNotRead :: MonadIO m => m ()
couldNotRead = do
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  UTF8.putText "🚫 Unable to read credentials. Try logging in with "

  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  UTF8.putText "fission-cli login"

  liftIO $ ANSI.setSGR [ANSI.Reset]

-- | Create a could not connect to Fission peer message for the terminal
couldNotSwarmConnect :: MonadIO m => m ()
couldNotSwarmConnect = do
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  UTF8.putText "😭 We were unable to connect to the Fission IPFS peer!"

  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  UTF8.putText "Try checking your connection or logining in again"

  liftIO $ ANSI.setSGR [ANSI.Reset]

-- | Removes the users config file
removeConfigFile :: MonadUnliftIO m => m (Either IOException ())
removeConfigFile = do
  path <- cachePath
  try $ removeFile path