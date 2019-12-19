module Fission.Web.Client.DNS (update) where

import           Fission.Prelude

import           Servant.Client
import           Servant.Client.Core

import           Network.IPFS.CID.Types
import qualified Fission.Web.Routes           as Routes
import qualified Fission.Web.Auth.Types           as Auth

import           Fission.Web.Client

import qualified Fission.URL.DomainName.Types as URL

update :: CID -> ClientM URL.DomainName
update = withSigAuth update'

update' :: AuthenticatedRequest (Auth.HigherOrder) -> CID -> ClientM URL.DomainName
update' = client <| Proxy @Routes.DNSRoute
