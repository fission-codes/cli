-- | Pin files via the CLI
module Fission.CLI.Pin
  ( pin
  , run
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Data.List.NonEmpty

import Servant
import Servant.Client

import qualified Fission.Config as Config
import           Fission.Internal.Constraint

import qualified Fission.IPFS.Peer    as IPFS.Peer
import qualified Fission.IPFS.Types   as IPFS
import           Fission.IPFS.CID.Types

import qualified Fission.Web.Client      as Client
import qualified Fission.Web.Client.IPFS as Fission

import           Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Loader  as CLI
import           Fission.CLI.Display.Success as CLI.Success
import           Fission.CLI.Config.Types.LoggedIn

run :: MonadRIO          cfg m
    => HasLoggedIn  cfg
    => CID
    -> m (Either ClientError CID)
run cid@(CID hash)  = do
  logDebug $ "Remote pinning " <> display hash

  Client.Runner runner <- Config.get
  peer <- Config.get
  auth <- Config.get
  -- Question: What would be the best way to bring this up further?
  -- like a HasSwarmConnection
  IPFS.Peer.connect peer


  liftIO (pin runner auth cid) >>= \case
    Right _ -> do
      CLI.Success.live hash
      return $ Right cid

    Left err -> do
      CLI.Error.put' err
      return $ Left err

pin :: MonadUnliftIO m => (ClientM NoContent -> m a) -> BasicAuthData -> CID -> m a
pin runner auth cid = CLI.withLoader 50000 . runner $ Fission.pin (Fission.request auth) cid
