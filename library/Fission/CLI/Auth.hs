module Fission.CLI.Auth
  ( cachePath
  , get
  , write
  , couldNotRead
  , removeConfigFile
  ) where

import           RIO           hiding (set)
import           RIO.Directory
import           RIO.File
import           RIO.FilePath

import qualified System.Console.ANSI as ANSI

import           Data.Has
import qualified Data.Yaml as YAML
import           Servant
import           Servant.Client

import           Fission.Internal.Constraint
import           Fission.Internal.Orphanage.BasicAuthData ()
import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.IPFS.Types    as IPFS
import           Fission.CLI.Environment.Types
import           Data.List.NonEmpty as NonEmpty

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

couldNotRead :: MonadIO m => m ()
couldNotRead = do
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  UTF8.putText "🚫 Unable to read credentials. Try logging in with "

  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  UTF8.putText "fission-cli login"

  liftIO $ ANSI.setSGR [ANSI.Reset]

-- | Removes the users config file
removeConfigFile :: MonadUnliftIO m => m (Either IOException ())
removeConfigFile = do
  path <- cachePath
  try $ removeFile path
