-- | Pin files via the CLI
module Fission.CLI.IPFS.Pin
  ( pin
  , run
  ) where

import Fission.Prelude

import Servant
import Servant.Client

import qualified Fission.Config as Config

import           Network.IPFS.CID.Types

import qualified Fission.Web.Client      as Client
import qualified Fission.Web.Client.IPFS as Fission

import           Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Loader  as CLI
import           Fission.CLI.Display.Success as CLI.Success
import           Fission.CLI.Config.FissionConnected.Types

run ::
  ( MonadRIO             cfg m
  , HasFissionConnected  cfg
  )
  => CID
  -> m (Either ClientError CID)
run cid@(CID hash)  = do
  logDebug <| "Remote pinning " <> display hash

  Client.Runner runner <- Config.get
  auth <- Config.get

  liftIO (pin runner auth cid) >>= \case
    Right _ -> do
      CLI.Success.live hash
      return <| Right cid

    Left err -> do
      CLI.Error.put' err
      return <| Left err

pin :: MonadUnliftIO m => (ClientM NoContent -> m a) -> BasicAuthData -> CID -> m a
pin runner auth cid = CLI.withLoader 50000 . runner <| Fission.pin (Fission.request auth) cid
