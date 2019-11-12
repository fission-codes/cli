module Fission.CLI.Environment.Types
  ( Environment (..)
  , PartialEnv (..)
  ) where

import RIO

import           Data.Aeson
import           Servant.API

import qualified Fission.IPFS.Types as IPFS
import           Fission.Internal.Orphanage.BasicAuthData ()

import qualified Fission.CLI.Environment.Error as Error

data Environment = Environment
  { userAuth :: BasicAuthData
  , peers    :: Maybe (NonEmpty IPFS.Peer)
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
                
data PartialEnv = PartialEnv
    { maybeUserAuth :: Maybe BasicAuthData
    , maybePeers    :: Maybe (NonEmpty IPFS.Peer)
    } 

instance ToJSON PartialEnv where
  toJSON PartialEnv {..} = object
    [ "user_auth" .= maybeUserAuth
    , "peers"     .= maybePeers 
    ]

instance FromJSON PartialEnv where
  parseJSON = withObject "PartialEnv" $ \obj ->
    PartialEnv <$> obj .:? "user_auth"
               <*> obj .:? "peers"

instance Semigroup PartialEnv where
  f <> g = PartialEnv {
    maybeUserAuth = preferSecond f g maybeUserAuth,
    maybePeers = preferSecond f g maybePeers
  }

instance Monoid PartialEnv where
  mempty = PartialEnv {
    maybeUserAuth = Nothing,
    maybePeers = Nothing
  }

preferSecond :: PartialEnv -> PartialEnv -> (PartialEnv -> Maybe a) -> Maybe a
preferSecond a b fn = maybe (fn a) Just (fn b)
