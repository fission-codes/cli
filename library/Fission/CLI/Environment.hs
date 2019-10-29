-- | Reading and writing local user config values
module Fission.CLI.Environment where

import RIO
import           Servant.API

import Data.Has

import qualified Fission.Config as Config
import           Fission.Internal.Constraint

import           Fission.Web.Client.Peers as Peers
import qualified Fission.Web.Client.Types as Client

import qualified Fission.CLI.Auth as Auth
import           Fission.CLI.Config.Types

import qualified Fission.CLI.Display.Cursor  as Cursor
import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait

init :: MonadRIO cfg m
          => MonadUnliftIO         m
          => HasLogFunc        cfg
          => Has Client.Runner cfg
          => BasicAuthData
          -> m ()
init auth = do
  Client.Runner run <- Config.get

  res <- liftIO
        $ Cursor.withHidden
        $ CLI.Wait.waitFor "ddddddd"
        $ run
        $ Peers.get

  case res of
    Left err ->
      CLI.Error.put err "Peer retrieval failed"

    Right peers -> do
      liftIO $ Auth.write auth peers -- Environment.write
      CLI.Success.putOk "Logged in"
