-- | Servant client for retrieving peer data
module Fission.Web.Client.Peers (get) where

import RIO

import Servant
import Servant.Client

import qualified Fission.IPFS.Types    as IPFS
import qualified Fission.Web.IPFS.Peer as Peer

-- | API path to the peers endpoints
type API = "ipfs" :> "peers" :> Peer.API

-- | Retrieve a list of peers from the fission api
get :: ClientM [IPFS.Peer]
get = client (Proxy :: Proxy API)
