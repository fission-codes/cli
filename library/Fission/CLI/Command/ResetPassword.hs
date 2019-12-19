-- | Register command
module Fission.CLI.Command.ResetPassword (command, resetPassword) where

import           Fission.Prelude
import           Data.Function

import           Servant

import           Options.Applicative.Simple (addCommand)

import qualified Fission.Internal.UTF8 as UTF8

import           Fission.Web.Client.Auth  as Client
import qualified Fission.Web.Client.User  as User.Client

import qualified Fission.User.Password.Types as User

import           Fission.CLI.Config.Types
import           Fission.CLI.Config.Base
import           Fission.CLI.Config.Connected

import qualified Fission.CLI.Display.Cursor  as Cursor
import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait
import qualified Fission.CLI.Prompt.Fields   as Fields

import qualified Fission.CLI.Environment               as Environment
import qualified Fission.CLI.Environment.Partial       as Env.Partial
import           Fission.CLI.Environment.Partial.Types as Env

-- | The command to attach to the CLI tree
command ::
  MonadIO m
  => BaseConfig
  -> CommandM (m ())
command cfg =
  addCommand
    "reset-password"
    "Reset Fission Password"
    (const <| void <| runConnected cfg resetPassword)
    (pure ())

-- | Register and login (i.e. save credentials to disk)
resetPassword ::
  ( MonadUnliftIO      m
  , MonadLogger        m
  , MonadAuthedClient m
  )
  => m ()
resetPassword = do
  newPassword <- Fields.getRequiredSecret "New Password"
  auth <- getAuth

  logDebugN "Attempting registration"

  let pw = User.Password <| UTF8.textShow newPassword

  resetResult <- Cursor.withHidden . CLI.Wait.waitFor "Registering..."
                  <| Client.run <| User.Client.resetPassword auth pw 

  case resetResult of
    Left  err ->
      CLI.Error.put err "Password Reset failed"

    Right (User.Password updatedPass) ->
      Environment.findLocalAuth >>= \case
        Left _ -> Environment.couldNotRead
        Right path -> do
          let
            updatedAuth = BasicAuthData
              { basicAuthUsername = basicAuthUsername auth
              , basicAuthPassword = encodeUtf8 updatedPass
              }
            updatedEnv = (mempty Env.Partial) { maybeUserAuth = Just updatedAuth}
          Env.Partial.writeMerge path updatedEnv
          CLI.Success.putOk <|
            "Password reset. Your updated credentials are in " <> UTF8.textShow path


