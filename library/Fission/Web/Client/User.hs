module Fission.Web.Client.User
  ( register
  , verify
  , updateDID
  , getEmail
  ) where

import Fission.Prelude

import Servant hiding (addHeader)
import Servant.Client
import Servant.Client.Core

import qualified Fission.User.Registration.Types as User
import qualified Fission.User.Username.Types     as User
import qualified Fission.User.Email.Types        as User
import qualified Fission.User.DID.Types        as User

import           Fission.Web.Client
import qualified Fission.Web.Client.JWT as JWT
import qualified Fission.Web.Client.BasicAuth as Auth

import qualified Fission.Web.Auth.Types as Auth

import           Fission.Web.Routes              (UserRoute)

register  :: User.Registration -> ClientM ()
verify    :: ClientM User.Username
updateDID :: BasicAuthData -> User.DID -> ClientM User.DID
getEmail  :: User.Username -> ClientM User.Email
register = withRegisterAuth register'
verify = JWT.getSigAuth >>= verify'
updateDID auth = updateDID' <| Auth.getBasicAuth auth 

register'  :: AuthenticatedRequest (Auth.RegisterDid) -> User.Registration -> ClientM ()
verify'    :: AuthenticatedRequest (Auth.HigherOrder) -> ClientM User.Username
updateDID' :: AuthenticatedRequest (Auth.HigherOrder) -> User.DID -> ClientM User.DID
register' :<|> verify' :<|> updateDID' :<|> getEmail :<|> _ =
  client (Proxy @UserRoute)
