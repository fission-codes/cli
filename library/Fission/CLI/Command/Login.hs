-- | Login command
module Fission.CLI.Command.Login (command, login) where

import           Fission.Prelude

import           Options.Applicative.Simple hiding (command)
import           Servant

import           Fission.Web.Client
import           Fission.Web.Client.User as User.Client

import qualified Fission.CLI.Environment               as Env
import           Fission.CLI.Environment.Partial.Types as Env
import qualified Fission.CLI.Environment.Partial       as Env.Partial

import           Fission.CLI.Config.Base
import           Fission.CLI.Config.Types

import           Fission.CLI.Command.Login.Types as Login
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
    "login"
    "Add your Fission credentials"
    (\options -> void <| runBase cfg <| login options)
    parseOptions

-- | Login (i.e. save credentials to disk). Validates credentials agianst the server.
login ::
  ( MonadUnliftIO         m
  , MonadWebClient        m
  , MonadLogger           m
  )
  => Login.Options
  -> m ()
login Login.Options {..} = do
  logDebugN "Starting login sequence"
  username <- getUsername username_option
  password <- getUserPassword password_option

  logDebugN "Attempting API verification"

  let auth = BasicAuthData username password

  authResult <- Cursor.withHidden
              . CLI.Wait.waitFor "Verifying your credentials"
              . run <| User.Client.verify auth

  case authResult of
    Left  err ->
      CLI.Error.put err "Authorization failed"

    Right _ok -> do
      logDebugN "Auth Successful"

      envPath <- Env.getPath local_auth

      if local_auth
        then
          mempty
            |> mayUserAuthLens ?~ auth
            |> Env.Partial.writeMerge envPath
        else Env.init auth

      CLI.Success.putOk <| "Successfully logged in. Your credentials are in " <> textShow envPath

-- | Get a users username, if not passed in via cli option prompt for input
getUsername ::
  ( MonadIO               m
  , MonadLogger           m
  )
  => Maybe ByteString
  -> m ByteString
getUsername (Just username) = return username
getUsername Nothing = Fields.getRequired "Username"

-- | Get a users password, if not passed in via cli option prompt for input
getUserPassword ::
  ( MonadIO               m
  , MonadLogger           m
  )
  => Maybe ByteString
  -> m ByteString
getUserPassword (Just option_password) = return option_password
getUserPassword Nothing = Fields.getRequiredSecret "Password"

parseOptions :: Parser Login.Options
parseOptions = do
  username_option <- optional <| strOption <| mconcat
    [ long    "user"
    , metavar "FISSION_USERNAME"
    , help    "The username to login with"
    ]

  password_option <- optional <| strOption <| mconcat
    [ long    "password"
    , metavar "FISSION_PASSWORD"
    , help    "The password to login with"
    ]

  local_auth <- switch <| mconcat
    [ long "local"
    , help "Login at project root (as opposed to global at user home)"
    ]

  return Login.Options {..}
