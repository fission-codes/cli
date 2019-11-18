-- | Register command
module Fission.CLI.Command.ResetPassword (command, resetPassword) where

import           RIO

import           Servant

import           Data.Has
import qualified Data.Text as T

import           Options.Applicative.Simple (addCommand)
import           System.Console.Haskeline

import qualified Fission.Config as Config
import           Fission.Internal.Constraint
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
import qualified Fission.CLI.Environment     as Environment

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
    (const $ void $ runRIO cfg $ LoggedIn.ensure resetPassword)
    (pure ())

-- | Register and login (i.e. save credentials to disk)
resetPassword :: MonadRIO       cfg m
              => MonadUnliftIO      m
              => HasLoggedIn    cfg
              => m ()
resetPassword = do
  auth <- Config.get
  liftIO (runInputT defaultSettings $ getPassword (Just '•') "New Password: ") >>= \case
    Nothing ->
      logError "Unable to read password"

    Just newPassword -> resetPassword' auth newPassword
            
resetPassword' :: MonadRIO          cfg m
               => MonadUnliftIO         m
               => HasLogFunc        cfg
               => Has Client.Runner cfg
               => BasicAuthData
               -> String
               -> m()
resetPassword' auth newPassword = do
  logDebug "Attempting registration"
  Client.Runner runner <- Config.get

  resetResult <- Cursor.withHidden
                  . liftIO
                  . CLI.Wait.waitFor "Registering..."
                  . runner
                  . User.Client.resetPassword auth
                  $ User.Password $ T.pack newPassword

  case resetResult of
    Left  err ->
      CLI.Error.put err "Password Reset failed"

    Right (User.Password updatedPass) -> do
      Environment.findLocalAuth >>= \case
        Left err -> CLI.Error.put err "Could"
        Right path -> do
          let
            updatedAuth = BasicAuthData
              { basicAuthUsername = basicAuthUsername auth
              , basicAuthPassword = encodeUtf8 updatedPass
              }
          Environment.writeAuth updatedAuth path
          CLI.Success.putOk $
            "Password reset. Your updated credentials are in " <> UTF8.textShow path


