-- | Update DNS via the CLI
module Fission.CLI.DNS (update) where

import Fission.Prelude

import Servant.Client

import           Fission.Web.Client.Auth
import qualified Fission.Web.DNS.Client  as DNS.Client

import           Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Loader  as CLI
import           Fission.CLI.Display.Success as CLI.Success

import           Network.IPFS.CID.Types
import qualified Fission.URL.DomainName.Types as URL

update ::
  ( MonadUnliftIO         m
  , MonadAuthedClient    m
  , MonadLogger           m
  )
  => CID
  -> m (Either ClientError URL.DomainName)
update cid@(CID hash) = do
  logDebug <| "Updating DNS to " <> display hash

  auth <- getAuth

  result <- CLI.withLoader 50000
            <| run
            <| DNS.Client.update auth cid

  case result of
    Right domain -> do
      CLI.Success.dnsUpdated <| URL.getDomainName domain
      return <| Right domain

    Left err -> do
      CLI.Error.put' err
      return <| Left err
