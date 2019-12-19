-- | Pin files via the CLI
module Fission.CLI.IPFS.Pin (add) where

import Fission.Prelude

import Servant.Client

import           Network.IPFS.CID.Types

import qualified Fission.Web.Client      as Client
import           Fission.Web.Client.Auth as Client
import qualified Fission.Web.Client.IPFS as Fission

import           Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Loader  as CLI
import           Fission.CLI.Display.Success as CLI.Success

add ::
  ( MonadUnliftIO            m
  , MonadLogger              m
  , MonadAuthedClient       m
  )
  => CID
  -> m (Either ClientError CID)
add cid@(CID hash)  = do
  logDebug <| "Remote pinning " <> display hash

  auth <- getAuth

  result <- CLI.withLoader 50000 
            <| Client.run
            <| Fission.pin (Fission.request auth) cid

  case result of 
    Right _ -> do
      CLI.Success.live hash
      return <| Right cid

    Left err -> do
      CLI.Error.put' err
      return <| Left err
