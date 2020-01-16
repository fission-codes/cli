-- | Setup command
module Fission.CLI.Command.Setup (command, setup) where

import           Fission.Prelude

import           Options.Applicative.Simple (addCommand)
import           Servant.API

import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Prompt          as Prompt

import qualified Fission.CLI.Environment.Partial as Env.Partial

import qualified Fission.Internal.UTF8 as UTF8

import           Fission.Web.Client       as Client
import qualified Fission.Web.Client.User  as User.Client

import qualified Fission.User.Username.Types     as User
import qualified Fission.User.Email.Types        as User
import qualified Fission.User.Registration.Types as User

import qualified Fission.User.DID as DID

import           Fission.CLI.Config.Types
import           Fission.CLI.Config.Base

import qualified Fission.Key.Store as Key

-- | The command to attach to the CLI tree
command ::
  MonadIO m
  => BaseConfig
  -> CommandM (m ())
command cfg =
  addCommand
    "setup"
    "Setup fission on your machine"
    (\_ -> runBase cfg setup)
    (pure ())

setup ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  )
  => m ()
setup =
  Key.exists >>= \case
    True -> do 
      authResult <- Client.run <| User.Client.verify
      case authResult of
        Left err -> CLI.Error.put err
          "We don't recognize your key! Please contact fission support or delete `~/.ssh/fission` and try again."
        Right (User.Username username) ->
          CLI.Success.loggedInAs <| username

    False ->
      Env.Partial.findBasicAuth >>= \case
        Nothing -> do
          username <- Prompt.reaskNotEmpty' "Username: "
          email <- Prompt.reaskNotEmpty' "Email: "
          createAccount username email
        Just auth -> 
          updateDID auth 

createAccount ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  )
  => Text
  -> Text
  -> m ()
createAccount username email = do
  emailResp <- Client.run <| User.Client.getEmail <| User.Username username

  case emailResp of
    Right (User.Email email') -> do
      if email == email'
      then do
        -- @@TODO: recover flow
        UTF8.putTextLn ""
        return ()
      else do
        UTF8.putTextLn "😕 That username's already taken. Try again?"
        username' <- Prompt.reaskNotEmpty' "Username: "
        createAccount username' email

    Left err ->
      case Client.is404 err of
        False -> 
          CLI.Error.put err "Could not reach Fission Server"
        True -> do
          -- If 404, user does not exist yet
          createKey
          UTF8.putText "📝 Registering your new account... "
          register username email

updateDID ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  )
  => BasicAuthData
  -> m ()
updateDID auth = do
  shouldUpgrade <- Prompt.reaskYN <| mconcat 
                [ "Upgrade account "
                , decodeUtf8Lenient (basicAuthUsername auth)
                , "? (y/n)"
                ]
  case shouldUpgrade of
    False -> return ()
    True -> do
      createKey
      UTF8.putText "📝 Upgrading your account... "
      Key.publicKeyEd >>= \case
        Left err ->
          CLI.Error.put err "Could not read key file"
        Right pubkey -> do
          let did = DID.fromPubkey pubkey
          updateResult <- Client.run <| User.Client.updateDID auth did
          case updateResult of
            Left err ->
              CLI.Error.put err "Could not upgrade account"
            Right _ok -> 
              CLI.Success.putOk "Upgrade successful!"

createKey :: MonadIO m => m ()
createKey = do
  UTF8.putText "🔑 Creating your key at ~/.ssh/fission... "
  Key.forceCreate
  UTF8.putTextLn "done"

register ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  )
  => Text
  -> Text
  -> m ()
register username email = do
  registerResult <- Client.run <| User.Client.register User.Registration {..}
  
  case registerResult of
    Left err ->
      CLI.Error.put err "Could not register account"
    Right _ok -> 
      CLI.Success.putOk "Registration successful!"
