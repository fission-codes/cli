module Fission.Web.Client.IPFS
  ( dagput
  , unpin
  , pin
  , upload
  , cids
  ) where

import           Fission.Prelude

import           Servant
import           Servant.Client

import qualified Network.IPFS.File.Types as File
import           Network.IPFS.CID.Types

import           Fission.Web.Routes (IPFSPrefix)

import           Fission.Web.Client

import qualified Fission.Web.IPFS               as IPFS
import qualified Fission.Web.IPFS.CID           as CID
import qualified Fission.Web.IPFS.Upload.Simple as Upload.Simple
import qualified Fission.Web.IPFS.Pin           as Pin
import qualified Fission.Web.IPFS.DAG           as DAG

dagput :: File.Serialized -> ClientM CID
dagput = withSigAuth <| client <| Proxy @(IPFS.Auth :> IPFSPrefix :> "dag" :> DAG.API)

unpin :: CID -> ClientM NoContent
unpin = withSigAuth <| client <| Proxy @(IPFS.Auth :> IPFSPrefix :> Pin.UnpinAPI)

pin :: CID -> ClientM NoContent
pin = withSigAuth <| client <| Proxy @(IPFS.Auth :> IPFSPrefix :> Pin.PinAPI)

upload :: File.Serialized -> ClientM CID
upload = withSigAuth <| client <| Proxy @(IPFS.Auth :> IPFSPrefix :> Upload.Simple.API)

cids :: ClientM [CID]
cids = withSigAuth' <| client <| Proxy @(IPFS.Auth :> IPFSPrefix :> "cids" :> CID.API)
