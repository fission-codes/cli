module Fission.CLI.Environment.Partial
  ( get
  , decode
  , write
  , writeMerge
  , toFull
  , fromFull
  , updatePeers
  ) where

import           Fission.Prelude hiding (decode)
import           RIO.Directory
import           RIO.File
import           RIO.FilePath

import qualified Data.Yaml as YAML
import           Data.List.NonEmpty as NonEmpty hiding ((<|))

import           Fission.CLI.Environment.Types
import           Fission.CLI.Environment.Partial.Types as Env
import qualified Fission.CLI.Environment.Error as Error

import           Fission.Internal.Orphanage.BasicAuthData ()

import qualified Fission.IPFS.Types as IPFS

-- | Gets hierarchical environment by recursed through file system
get :: MonadIO m => m Env.Partial
get = getRecurse =<< getCurrentDirectory

getRecurse :: MonadIO m => FilePath -> m Env.Partial
getRecurse "/" = decode <| "/.fission.yaml"
getRecurse path = do
  parent <- getRecurse <| takeDirectory path
  curr <- decode <| path </> ".fission.yaml"
  return <| parent <> curr

-- | Decodes file to partial environment
decode :: MonadIO m => FilePath -> m Env.Partial
decode path = liftIO <| YAML.decodeFileEither path >>= \case
  Left _ -> return <| mempty Env.Partial
  Right env -> return env

-- | Writes partial environment to path
write :: MonadIO m => FilePath -> Env.Partial -> m ()
write path env = writeBinaryFileDurable path <| YAML.encode env

-- | Merges partial env with the env at the path and overwrites
writeMerge :: MonadIO m => FilePath -> Env.Partial -> m ()
writeMerge path newEnv = do
  currEnv <- decode path
  writeBinaryFileDurable path <| YAML.encode <| currEnv <> newEnv

toFull :: Env.Partial -> (Either Error.Env Environment)
toFull partial =
  case maybeUserAuth partial of
    Nothing -> Left Error.EnvIncomplete
    Just basicAuth -> Right <| Environment
      { userAuth = basicAuth
      , peers = maybePeers partial
      , ignored = fromMaybe [] <| maybeIgnored partial
      , buildDir = maybeBuildDir partial
      }

fromFull :: Environment -> Env.Partial
fromFull env = Env.Partial
  { maybeUserAuth = Just <| userAuth env
  , maybePeers = peers env
  , maybeIgnored = Just <| ignored env
  , maybeBuildDir = buildDir env
  }

updatePeers :: Env.Partial -> [IPFS.Peer] -> Env.Partial
updatePeers env peers = env
  { maybePeers    = Just (NonEmpty.fromList peers) }
