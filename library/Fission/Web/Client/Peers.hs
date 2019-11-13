-- | Servant client for retrieving peer data
module Fission.Web.Client.Peers (getPeers) where

import RIO

import Servant
import Servant.Client
import Data.Has

import qualified Fission.Web.IPFS.Peer as Peer
import qualified Fission.Web.Client.Types as Client

import qualified Fission.IPFS.Types as IPFS

import qualified Fission.Config as Config

import qualified Fission.CLI.Display.Cursor as Cursor
import qualified Fission.CLI.Display.Wait as CLI.Wait

-- | API path to the peers endpoints
type API = "ipfs" :> "peers" :> Peer.API

-- | Retrieves the Fission peer list from the server
getPeers :: MonadReader cfg m
  => Has Client.Runner cfg
  => MonadIO m
  => m (Either ClientError [IPFS.Peer])
getPeers = do
  Client.Runner run <- Config.get
  liftIO
    $ Cursor.withHidden
    $ CLI.Wait.waitFor "Retrieving Fission Peer List..."
    $ run
    $ get

-- | Retrieve a list of peers from the fission api
get :: ClientM [IPFS.Peer]
get = client (Proxy :: Proxy API)
