-- | BEN FILL ME IN
module Fission.Web.Client.Peers (get) where

import RIO

import Servant
import Servant.Client

import qualified Fission.IPFS.Types    as IPFS
import qualified Fission.Web.IPFS.Peer as Peer

-- | WHAT'S THIS FOR?
type API = "ipfs" :> "peers" :> Peer.API

-- | DESCRIBE ME!
get :: ClientM [IPFS.Peer]
get = client (Proxy :: Proxy API)
