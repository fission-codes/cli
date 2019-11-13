module Fission.CLI.Environment.Partial.Types ( Partial (..)) where

import RIO

import           Data.Aeson
import           Servant.API

import qualified Fission.IPFS.Types as IPFS
import           Fission.Internal.Orphanage.BasicAuthData ()
                
data Partial = Partial
    { maybeUserAuth :: Maybe BasicAuthData
    , maybePeers    :: Maybe (NonEmpty IPFS.Peer)
    } 

instance ToJSON Partial where
  toJSON Partial {..} = object $ catMaybes
    [ maybe Nothing (Just . makePair "user_auth") maybeUserAuth
    , maybe Nothing (Just . makePair "peers") maybePeers 
    ]

instance FromJSON Partial where
  parseJSON = withObject "Partial" $ \obj ->
    Partial <$> obj .:? "user_auth"
               <*> obj .:? "peers"

instance Semigroup Partial where
  f <> g = Partial {
    maybeUserAuth = getField maybeUserAuth f g,
    maybePeers = getField maybePeers f g
  }

instance Monoid Partial where
  mempty = Partial {
    maybeUserAuth = Nothing,
    maybePeers = Nothing
  }

makePair :: (KeyValue kv, ToJSON v) => Text -> v -> kv
makePair a b = a .= b

getField :: (Partial -> Maybe field) -> Partial -> Partial -> Maybe field
getField accessor a b = maybe (accessor a) Just (accessor b)
