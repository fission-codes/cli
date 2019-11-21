-- | Login command
module Fission.CLI.Command.Login (command, login) where

import           Fission.Prelude
import           RIO.ByteString

import qualified Data.ByteString.Char8 as BS

import           Options.Applicative.Simple (addCommand)
import           Servant
import           System.Console.Haskeline

import qualified Fission.Config as Config

import           Fission.Web.Client.User as User.Client
import qualified Fission.Web.Client.Types as Client
import qualified Fission.CLI.Environment as Environment

import           Fission.CLI.Config.Types

import           Fission.CLI.Command.Login.Types as Login
import qualified Fission.CLI.Display.Cursor  as Cursor
import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait

-- | The command to attach to the CLI tree
command :: MonadIO m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "login"
    "Add your Fission credentials"
    (const <| runRIO cfg login)
    parseOptions

-- | Login (i.e. save credentials to disk). Validates credentials agianst the server.
login :: MonadRIO          cfg m
      => MonadUnliftIO         m
      => HasLogFunc        cfg
      => Has Client.Runner cfg
      => Login.Options ()
      -> m ()
login Login.Options {..} = do
  logDebug "Starting login sequence"
  logDebug username2
  putStr "Username: "
  username <- getLine
  liftIO (runInputT defaultSettings <| getPassword (Just '•') "Password: ") >>= \case
    Nothing ->
      logError "Unable to read password"

    Just password -> do
      logDebug "Attempting API verification"
      Client.Runner run <- Config.get
      let auth = BasicAuthData username <| BS.pack password

      authResult <- Cursor.withHidden
                  . liftIO
                  . CLI.Wait.waitFor "Verifying your credentials"
                  . run <| User.Client.verify auth

      case authResult of
        Left  err ->
          CLI.Error.put err "Authorization failed"

        Right _ok -> do
          logDebug "Auth Successful"

          Environment.init auth
          CLI.Success.putOk "Registered & logged in. Your credentials are in ~/.fission.yaml"

parseOptions :: Parser Login.Options
parseOptions = do

  username2 <- strArgument <| mconcat
    [ metavar "FISSION_USERNAME"
    , long "username2"
    , short "u"
    , help    "Specifcy the FIssion user you'd like to login as"
    ]

  return Login.Options {..}

