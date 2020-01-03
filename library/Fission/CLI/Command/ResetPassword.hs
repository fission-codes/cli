-- | Register command
module Fission.CLI.Command.ResetPassword (command, resetPassword) where

import           Fission.Prelude
import           Data.Function

import           Servant

import           Options.Applicative.Simple (addCommand)

import qualified Fission.Config as Config
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.Web.Client.User  as User.Client
import qualified Fission.Web.Client.Types as Client

import qualified Fission.User.Password.Types as User

import           Fission.CLI.Config.Types
import           Fission.CLI.Config.LoggedIn as LoggedIn

import qualified Fission.CLI.Display.Cursor  as Cursor
import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait
import qualified Fission.CLI.Prompt.Fields   as Fields

import qualified Fission.CLI.Environment               as Environment
import qualified Fission.CLI.Environment.Partial       as Env.Partial
import           Fission.CLI.Environment.Partial.Types as Env

-- | The command to attach to the CLI tree
command :: MonadUnliftIO m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "reset-password"
    "Reset Fission Password"
    (const <| void <| runRIO cfg <| LoggedIn.ensure resetPassword)
    (pure ())

-- | Register and login (i.e. save credentials to disk)
resetPassword ::
  ( MonadReader    cfg m
  , MonadUnliftIO      m
  , MonadLogger        m
  , HasLoggedIn    cfg
  )
  => m ()
resetPassword = do
  auth <- Config.get
  newPassword <- Fields.getRequiredSecret "New Password"
  resetPassword' auth newPassword

resetPassword' ::
  ( MonadReader    cfg m
  , MonadUnliftIO      m
  , MonadLogger        m
  , Has Client.Runner cfg
  )
  => BasicAuthData
  -> ByteString
  -> m()
resetPassword' auth newPassword = do
  logDebugN "Attempting registration"
  Client.Runner runner <- Config.get

  resetResult <- Cursor.withHidden
                  . liftIO
                  . CLI.Wait.waitFor "Registering..."
                  . runner
                  . User.Client.resetPassword auth
                  <| User.Password
                  <| UTF8.textShow newPassword

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


