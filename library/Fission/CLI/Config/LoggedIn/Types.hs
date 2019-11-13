-- | Configuration required to run any CLI function that interacts with the Fission service
module Fission.CLI.Config.LoggedIn.Types
  ( LoggedIn (..)
  , HasLoggedIn
  , fissionAPI
  , logFunc
  , userAuth
  ) where

import RIO
import Servant.API

import Data.Has
import Control.Lens (makeLenses)

import qualified Fission.Web.Client.Types as Client

type HasLoggedIn cfg
  = ( HasLogFunc        cfg
    , Has Client.Runner cfg
    , Has BasicAuthData cfg
    )

data LoggedIn = LoggedIn
  { _fissionAPI  :: !Client.Runner
  , _logFunc     :: !LogFunc
  , _userAuth    :: !BasicAuthData
  }

makeLenses ''LoggedIn

instance Has Client.Runner LoggedIn where
  hasLens = fissionAPI

instance HasLogFunc LoggedIn where
  logFuncL = logFunc

instance Has BasicAuthData LoggedIn where
  hasLens = userAuth
