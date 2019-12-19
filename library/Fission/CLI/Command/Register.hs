-- | Register command
module Fission.CLI.Command.Register (command, register) where
import           Fission.Prelude

import           Options.Applicative.Simple hiding (command)
import           Servant

import           Fission.Web.Client       as Client
import qualified Fission.Web.Client.User  as User.Client

import qualified Fission.User.Registration.Types as User

import qualified Fission.CLI.Environment               as Env
import           Fission.CLI.Environment.Partial.Types as Env
import qualified Fission.CLI.Environment.Partial       as Env.Partial

import           Fission.CLI.Config.Base
import           Fission.CLI.Config.Types

import           Fission.CLI.Command.Register.Types as Register
import qualified Fission.CLI.Display.Cursor  as Cursor
import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait
import qualified Fission.CLI.Prompt.Fields   as Fields

-- | The command to attach to the CLI tree
command ::
  MonadIO m
  => BaseConfig
  -> CommandM (m ())
command cfg =
  addCommand
    "register"
    "Register for Fission and login"
    (\options -> void <| runBase cfg <| register options)
    parseOptions

-- | Register and login (i.e. save credentials to disk)
register ::
  ( MonadUnliftIO         m
  , MonadWebClient         m
  , MonadLogger           m
  )
  => Register.Options
  -> m ()
register Register.Options {..} = do
  envPath <- Env.getPath local_auth
  env <- Env.Partial.decode envPath
  case maybeUserAuth env of
    Nothing -> register' local_auth
    Just _ ->
      CLI.Success.putOk <| mconcat
        [ "Already registered. Remove your credentials at "
        ,  textShow envPath
        , " if you want to re-register"]

register' ::
  ( MonadUnliftIO  m
  , MonadWebClient m
  , MonadLogger    m
  )
  => Bool
  -> m ()
register' local_auth = do
  logDebugN "Starting registration sequence"

  username <- Fields.getRequired "Username"
  password <- Fields.getRequiredSecret "Password"
  rawEmail <- Fields.getRequired "Email"

  logDebugN "Attempting registration"

  let user = User.Registration
              { username = decodeUtf8Lenient username
              , password = decodeUtf8Lenient password
              , email    = decodeUtf8Lenient rawEmail
              }

  registerResult <- Cursor.withHidden . CLI.Wait.waitFor "Registering..."
                    <| Client.run <| User.Client.register user

  case registerResult of
    Left  err ->
      CLI.Error.put err "Authorization failed"

    Right _ok -> do
      logDebugN "Register Successful"

      let auth = BasicAuthData username password
      envPath <- Env.getPath local_auth

      if local_auth
        then Env.Partial.writeMerge envPath
          <| (mempty Env.Partial) { maybeUserAuth = Just auth }
        else Env.init auth

      CLI.Success.putOk <| "Registered & logged in. Your credentials are in " <> textShow envPath

parseOptions :: Parser Register.Options
parseOptions = do
  local_auth <- switch <| mconcat
    [ long "local"
    , help "Register at project root (as opposed to global at user home)"
    ]

  return Register.Options {..}
