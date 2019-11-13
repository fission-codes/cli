module Fission.CLI.Environment.Partial
  ( get
  , recurseEnv
  , decode
  , write
  , toFull
  , fromFull
  , updateAuth
  , updatePeers
  ) where

import           RIO           hiding (set)
import           RIO.Directory
import           RIO.File
import           RIO.FilePath
import           Servant.API

import qualified Data.Yaml as YAML
import           Data.List.NonEmpty as NonEmpty

import           Fission.CLI.Environment.Types
import           Fission.CLI.Environment.Partial.Types as Env
import qualified Fission.CLI.Environment.Error as Error

import           Fission.Internal.Orphanage.BasicAuthData ()

import qualified Fission.IPFS.Types as IPFS

-- | Gets hierarchical environment by recursed through file system
get :: MonadIO m => m Env.Partial
get = recurseEnv =<< getCurrentDirectory

recurseEnv :: MonadIO m => FilePath -> m Env.Partial
recurseEnv "/" = decode $ "/.fission.yaml"
recurseEnv path = do
  parent <- recurseEnv $ takeDirectory path
  curr <- decode $ path </> ".fission.yaml"
  return $ parent <> curr

-- | Decodes file to partial environment
decode :: MonadIO m => FilePath -> m Env.Partial
decode path = liftIO $ YAML.decodeFileEither path >>= \case
  Left _ -> return $ mempty Env.Partial
  Right env -> return env

-- | Writes partial environment to path
write :: MonadIO m => Env.Partial -> FilePath -> m ()
write env path = writeBinaryFileDurable path $ YAML.encode env

toFull :: Env.Partial -> (Either Error.Env Environment)
toFull partial =
  case maybeUserAuth partial of
    Nothing -> Left Error.EnvIncomplete
    Just basicAuth -> Right $ Environment
      { userAuth = basicAuth
      , peers = maybePeers partial
      }

fromFull :: Environment -> Env.Partial
fromFull env = Env.Partial
  { maybeUserAuth = Just $ userAuth env
  , maybePeers = peers env
  }

updateAuth :: Env.Partial -> BasicAuthData -> Env.Partial
updateAuth env auth = Env.Partial
  { maybeUserAuth = Just auth
  , maybePeers = maybePeers env
  }

updatePeers :: Env.Partial -> [IPFS.Peer] -> Env.Partial
updatePeers env peers = Env.Partial
  { maybeUserAuth = maybeUserAuth env
  , maybePeers = Just (NonEmpty.fromList peers)
  }
