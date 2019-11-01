module Main (main) where

import           RIO
import qualified RIO.Partial as Partial
import           RIO.Process (mkDefaultProcessContext)

import           Network.HTTP.Client     as HTTP
import           Network.HTTP.Client.TLS as HTTP
import           Servant.Client
import           System.Environment (lookupEnv)

import           Fission.Environment
import qualified Fission.Web.Client as Client

import qualified Fission.IPFS.BinPath.Types as IPFS
import qualified Fission.IPFS.Timeout.Types as IPFS

import           Fission.CLI
import qualified Fission.CLI.Config.Types   as CLI
import qualified Fission.CLI.Auth as Auth
import           Fission.CLI.Environment.Types

main :: IO ()
main = do
  verbose     <- isJust <$> lookupEnv "RIO_VERBOSE"
  logOptions  <- logOptionsHandle stderr verbose
  _processCtx <- mkDefaultProcessContext

  _ipfsPath    <- withEnv "IPFS_PATH" (IPFS.BinPath "/usr/local/bin/ipfs") IPFS.BinPath
  _ipfsTimeout <- withEnv "IPFS_TIMEOUT" (IPFS.Timeout 3600) (IPFS.Timeout . Partial.read)

  isTLS <- getFlag "FISSION_TLS" .!~ True
  path  <- withEnv "FISSION_ROOT" "" id
  host  <- withEnv "FISSION_HOST" "runfission.com" id
  port  <- withEnv "FISSION_PORT" (if isTLS then 443 else 80) Partial.read
  tOut  <- withEnv "FISSION_TIMEOUT" 1800000000 Partial.read
  eitherConfig <- Auth.get
  let rawHTTPSettings = if isTLS
                           then tlsManagerSettings
                           else defaultManagerSettings

  httpManager <- HTTP.newManager $ rawHTTPSettings
    { managerResponseTimeout = responseTimeoutMicro tOut }

  let url         = BaseUrl (if isTLS then Https else Http) host port path
      _fissionAPI = Client.Runner $ Client.request httpManager url
      _peers = case eitherConfig of
        Right config -> Just $ (peers config)
        Left _err -> Nothing

      _userAuth = case eitherConfig of
        Right config -> Just $ (userAuth config)
        Left _err -> Nothing

  withLogFunc logOptions \_logFunc -> runRIO CLI.Config {..} do
    logDebug $ "Requests will be made to " <> displayShow url
    cli
