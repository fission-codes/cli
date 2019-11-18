module Fission.Web.Client.Register
  ( register
  , verify
  , reset
  ) where

import           RIO

import           Servant
import           Servant.Client

import qualified Fission.User.Registration.Types          as User
import           Fission.Web.Routes                       (UserRoute)

import           Fission.User.Password.Types
import           Fission.Web.User.Password.Reset.Types

import           Fission.Internal.Orphanage.BasicAuthData ()

register :: User.Registration      -> ClientM ()
verify   :: BasicAuthData          -> ClientM Bool
reset    :: BasicAuthData -> Reset -> ClientM Password

register :<|> verify :<|> reset = client (Proxy :: Proxy UserRoute)
