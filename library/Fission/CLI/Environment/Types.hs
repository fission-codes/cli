module Fission.CLI.Environment.Types (Environment (..)) where

import Fission.Prelude

import           Servant.API

import qualified Fission.IPFS.Types as IPFS
import           Fission.Internal.Orphanage.BasicAuthData ()
import           Fission.Internal.Orphanage.Glob.Pattern ()

data Environment = Environment
  { userAuth :: BasicAuthData
  , peers    :: Maybe (NonEmpty IPFS.Peer)
  , ignored  :: IPFS.Ignored
  }

instance ToJSON Environment where
  toJSON Environment {..} = object
    [ "user_auth" .= userAuth
    , "peers"     .= peers
    , "ignore"    .= ignored
    ]

instance FromJSON Environment where
  parseJSON = withObject "Environment" <| \obj ->
    Environment <$> obj .: "user_auth"
                <*> obj .: "peers"
                <*> obj .: "ignore"
