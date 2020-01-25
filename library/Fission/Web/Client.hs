module Fission.Web.Client
  ( request
  , withSigAuth
  , withSigAuth'
  , withBasicAuth
  , withRegisterAuth
  , is404
  , module Fission.Web.Client.Types
  , module Fission.Web.Client.Class
  ) where

import Fission.Prelude

import           Servant
import           Servant.Client
import           Servant.Client.Core

import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types.Status

import           Fission.Web.Client.Types
import           Fission.Web.Client.Class
import           Fission.Web.Client.BasicAuth as Auth

import           Fission.Web.Client.JWT as JWT
import           Fission.Web.Auth.Types as Auth

request :: HTTP.Manager -> BaseUrl -> ClientM a -> IO (Either ClientError a)
request manager url query = runClientM query <| mkClientEnv manager url

withSigAuth ::
  (AuthenticatedRequest (Auth.HigherOrder) -> a -> ClientM b)
  -> (a -> ClientM b)
withSigAuth f x  = JWT.getSigAuth >>= flip f x

withSigAuth' ::
  (AuthenticatedRequest (Auth.HigherOrder) -> ClientM b)
  -> ClientM b
withSigAuth' f = JWT.getSigAuth >>= f

withBasicAuth ::
  (AuthenticatedRequest (Auth.HigherOrder) -> a -> ClientM b)
  -> (BasicAuthData -> a -> ClientM b)
withBasicAuth f auth x = f (Auth.getBasicAuth auth) x

withRegisterAuth ::
  (AuthenticatedRequest (Auth.RegisterDid) -> a -> ClientM b)
  -> (a -> ClientM b)
withRegisterAuth f x = JWT.getRegisterAuth >>= flip f x

is404 :: ClientError -> Bool
is404 (FailureResponse _ resp) = responseStatusCode resp == status404
is404 _ = False
