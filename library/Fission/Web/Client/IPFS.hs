module Fission.Web.Client.IPFS
  ( API
  , SimpleAPI
  , dagput
  , unpin
  , pin
  , upload
  , cids
  ) where

import           Fission.Prelude

import           Servant
import           Servant.Client
import           Servant.Client.Core

import qualified Network.IPFS.File.Types as File
import           Network.IPFS.CID.Types

import           Fission.Web.Routes (IPFSPrefix)

import           Fission.Web.Client
import qualified Fission.Web.Auth.Types as Auth
import qualified Fission.Web.Client.JWT        as JWT

import qualified Fission.Web.IPFS               as IPFS
import qualified Fission.Web.IPFS.CID           as CID
import qualified Fission.Web.IPFS.Upload.Simple as Upload.Simple
import qualified Fission.Web.IPFS.Pin           as Pin
import qualified Fission.Web.IPFS.DAG           as DAG

type API = IPFSPrefix :> SimpleAPI

type SimpleAPI = "cids" :> IPFS.Auth :> CID.API
            :<|> IPFS.Auth :> Upload.Simple.API
            :<|> IPFS.Auth :> Pin.PinAPI
            :<|> IPFS.Auth :> Pin.UnpinAPI
            :<|> "dag" :> IPFS.Auth :> DAG.API

dagput :: File.Serialized -> ClientM CID
unpin  :: CID             -> ClientM NoContent
pin    :: CID             -> ClientM NoContent
upload :: File.Serialized -> ClientM CID
cids   :: ClientM [CID]
dagput = withSigAuth dagput'
unpin  = withSigAuth unpin'
pin    = withSigAuth pin'
upload = withSigAuth upload'
cids   = JWT.getSigAuth >>= cids'

dagput' :: AuthenticatedRequest (Auth.HigherOrder) -> File.Serialized -> ClientM CID
unpin'  :: AuthenticatedRequest (Auth.HigherOrder) -> CID             -> ClientM NoContent
pin'    :: AuthenticatedRequest (Auth.HigherOrder) -> CID             -> ClientM NoContent
upload' :: AuthenticatedRequest (Auth.HigherOrder) -> File.Serialized -> ClientM CID
cids'   :: AuthenticatedRequest (Auth.HigherOrder) -> ClientM [CID]

cids' :<|> upload' :<|> pin' :<|> unpin' :<|> dagput' = client (Proxy @API)
