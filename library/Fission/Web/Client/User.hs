module Fission.Web.Client.User
  ( register
  , verify
  , getEmail
  ) where

import Fission.Prelude

import Servant hiding (addHeader)
import Servant.Client
import Servant.Client.Core

import qualified Fission.User.Registration.Types as User
import qualified Fission.User.Username.Types     as User
import qualified Fission.User.Email.Types        as User

import           Fission.Web.Client
import qualified Fission.Web.Client.JWT as JWT
import qualified Fission.Web.Auth.Types as Auth

import           Fission.Web.Routes              (UserRoute)

register :: User.Registration -> ClientM ()
verify   :: ClientM User.Username
getEmail :: User.Username -> ClientM User.Email
register = withRegisterAuth register'
verify = JWT.getSigAuth >>= verify'

register'      :: AuthenticatedRequest (Auth.RegisterDid) -> User.Registration -> ClientM ()
verify'        :: AuthenticatedRequest (Auth.HigherOrder) -> ClientM User.Username
register' :<|> verify' :<|> getEmail :<|> _ = client (Proxy @UserRoute)
