module Fission.Web.Client.User
  ( register
  , verify
  , updateDID
  ) where

import           Servant hiding (addHeader)
import           Servant.Client

import           Fission.Prelude

import qualified Fission.User.Registration.Types as User
import qualified Fission.User.Username.Types     as User
import qualified Fission.User.DID.Types          as User

import           Fission.Web.Client
import qualified Fission.Web.User   as User
import           Fission.Web.Routes (UserPrefix)

register  :: User.Registration -> ClientM NoContent
register = registerClient <| Proxy @(UserPrefix :> User.RegisterRoute)

verify :: ClientM User.Username
verify = sigClient' <| Proxy @(UserPrefix :> User.VerifyRoute)

updateDID :: BasicAuthData -> User.DID -> ClientM NoContent
updateDID = basicClient <| Proxy @(UserPrefix :> User.UpdateDIDRoute)
