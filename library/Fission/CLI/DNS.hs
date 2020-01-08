-- | Update DNS via the CLI
module Fission.CLI.DNS (update) where

import Fission.Prelude

import Servant
import Servant.Client

import qualified Fission.Config as Config

import qualified Fission.Web.Client      as Client
import qualified Fission.Web.DNS.Client  as DNS.Client

import           Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Loader  as CLI
import           Fission.CLI.Display.Success as CLI.Success

import           Network.IPFS.CID.Types
import qualified Fission.URL.DomainName.Types as URL

update ::
  ( MonadReader       cfg m
  , MonadIO               m
  , MonadLogger           m
  , Has Client.Runner cfg
  , Has BasicAuthData cfg
  )
  => CID
  -> m (Either ClientError URL.DomainName)
update cid@(CID hash) = do
  auth <- Config.get
  logDebug <| "Updating DNS to " <> display hash

  Client.Runner runner <- Config.get
  update' runner auth cid >>= \case
    Right domain -> do
      CLI.Success.dnsUpdated <| URL.getDomainName domain
      return <| Right domain

    Left err -> do
      CLI.Error.put' err
      return <| Left err

update' :: MonadIO m
        => (ClientM URL.DomainName -> IO a)
        -> BasicAuthData
        -> CID
        -> m a
update' runner auth cid =
  liftIO . CLI.withLoader 50000
         . runner
         <| DNS.Client.update auth cid
