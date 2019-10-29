module Fission.CLI.Environment.Types (Environment (..)) where

import RIO

import           Data.Aeson
import           Servant.API

import qualified Fission.IPFS.Types as IPFS
import           Fission.Internal.Orphanage.BasicAuthData ()

data Environment = Environment
  { userAuth :: BasicAuthData
  , peers    :: NonEmpty IPFS.Peer
  }

instance ToJSON Environment where
  toJSON Environment {..} = object
    [ "user_auth" .= userAuth
    , "peers"     .= peers
    ]

instance FromJSON Environment where
  parseJSON = withObject "Environment" $ \obj ->
    Environment <$> obj .: "user_auth"
                <*> obj .: "peers"