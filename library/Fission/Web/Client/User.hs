module Fission.Web.Client.User
  ( resetPassword
  , register
  , verify
  ) where

import RIO

import Servant
import Servant.Client

import qualified Fission.User.Registration.Types as User
import qualified Fission.User.Password.Types     as User
import           Fission.Web.Routes              (UserRoute)

import           Fission.Internal.Orphanage.BasicAuthData ()

verify        :: BasicAuthData     -> ClientM Bool
register      :: User.Registration -> ClientM ()
resetPassword :: BasicAuthData -> User.Password -> ClientM (User.Password)

register :<|> verify :<|> resetPassword = client (Proxy :: Proxy UserRoute)

