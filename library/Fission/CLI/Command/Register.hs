-- | Register command
module Fission.CLI.Command.Register (command, register) where

import           Fission.Prelude
import           RIO.ByteString
import           RIO.Directory
import           RIO.FilePath

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import           Options.Applicative.Simple hiding (command)
import           Servant
import           System.Console.Haskeline

import qualified Fission.Config as Config

import qualified Fission.Web.Client.User  as User.Client
import qualified Fission.Web.Client.Types as Client

import qualified Fission.User.Registration.Types as User

import qualified Fission.CLI.Environment               as Env
import           Fission.CLI.Environment.Partial.Types as Env
import qualified Fission.CLI.Environment.Partial       as Env.Partial

import           Fission.CLI.Config.Types

import           Fission.CLI.Command.Register.Types as Register
import qualified Fission.CLI.Display.Cursor  as Cursor
import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait

-- | The command to attach to the CLI tree
command :: MonadUnliftIO m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "register"
    "Register for Fission and login"
    (\options -> void <| runRIO cfg <| register options)
    parseOptions

-- | Register and login (i.e. save credentials to disk)
register :: MonadRIO       cfg m
        => MonadUnliftIO         m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => Register.Options
        -> m ()
register Register.Options {..} = do
  envPath <-
    if local_auth
    then getCurrentDirectory >>= \dir -> return <| dir </> ".fission.yaml"
    else Env.Partial.globalEnv
  
  env <- Env.Partial.decode envPath
  case maybeUserAuth env of
    Nothing -> register' local_auth
    Just _ -> 
      CLI.Success.putOk <| mconcat
        [ "Already registered. Remove your credentials at "
        ,  textShow envPath 
        , " if you want to re-register"]

register' :: MonadRIO cfg m
          => MonadUnliftIO         m
          => HasLogFunc        cfg
          => Has Client.Runner cfg
          => Bool
          -> m ()
register' local_auth = do
  logDebug "Starting registration sequence"

  putStr "Username: "
  username <- getLine

  liftIO (runInputT defaultSettings <| getPassword (Just '•') "Password: ") >>= \case
    Nothing ->
      logError "Unable to read password"

    Just password -> do
      putStr "Email: "
      rawEmail <- getLine

      logDebug "Attempting registration"
      Client.Runner runner <- Config.get

      registerResult <- Cursor.withHidden
                      . liftIO
                      . CLI.Wait.waitFor "Registering..."
                      . runner
                      . User.Client.register
                      <| User.Registration
                          { username = decodeUtf8Lenient username
                          , password = T.pack password
                          , email    = if BS.null rawEmail
                                          then Nothing
                                          else Just <| decodeUtf8Lenient rawEmail
                          }

      case registerResult of
        Left  err ->
          CLI.Error.put err "Authorization failed"

        Right _ok -> do
          logDebug "Register Successful"

          let auth = BasicAuthData username (BS.pack password)

          if local_auth
          then do
            currDir <- getCurrentDirectory
            let 
              envPath    = currDir </> ".fission.yaml"
              updatedEnv = (mempty Env.Partial) { maybeUserAuth = Just auth }
            Env.Partial.writeMerge envPath updatedEnv
            CLI.Success.putOk 
              <| "Registered & logged in. Your credentials are in " <> textShow envPath
          else do
            Env.init auth
            CLI.Success.putOk "Registered & logged in. Your credentials are in ~/.fission.yaml"

parseOptions :: Parser Register.Options
parseOptions = do
  local_auth <- switch <| mconcat
    [ long "local"
    , help "Register at project root (as opposed to global at user home)"
    ]

  return Register.Options {..}
