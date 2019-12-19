module Fission.Web.Client
  ( request
  , withAuth
  , module Fission.Web.Client.Types
  , module Fission.Web.Client.Class
  ) where

import Fission.Prelude

import qualified Network.HTTP.Client as HTTP
import           Servant
import           Servant.Client
import           Fission.Web.Client.Types
import           Fission.Web.Client.Class

withAuth ::
  ( HasClient ClientM api
  , Client ClientM api ~ (BasicAuthData -> clients)
  )
  => BasicAuthData
  -> Proxy api
  -> clients
withAuth basicAuth proxy = client proxy basicAuth

request :: HTTP.Manager -> BaseUrl -> ClientM a -> IO (Either ClientError a)
request manager url query = runClientM query <| mkClientEnv manager url
