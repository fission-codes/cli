module Fission.CLI.Environment.Partial.Types ( Partial (..)) where

import Fission.Prelude

import           Servant.API

import qualified Fission.IPFS.Types as IPFS
import           Fission.Internal.Orphanage.BasicAuthData ()
                
data Partial = Partial
  { maybeUserAuth :: Maybe BasicAuthData
  , maybePeers    :: Maybe (NonEmpty IPFS.Peer)
  } 

instance ToJSON Partial where
  toJSON Partial {..} = object <| catMaybes
    [ ("user_auth" .=) <$> maybeUserAuth
    , ("peers" .=) <$> maybePeers
    ]

instance FromJSON Partial where
  parseJSON = withObject "Partial" <| \obj ->
    Partial <$> obj .:? "user_auth"
            <*> obj .:? "peers"

instance Semigroup Partial where
  a <> b = Partial {
    maybeUserAuth = getField maybeUserAuth a b,
    maybePeers = getField maybePeers a b
  }

instance Monoid Partial where
  mempty = Partial
    { maybeUserAuth = Nothing
    , maybePeers = Nothing
    }

getField :: (Partial -> Maybe field) -> Partial -> Partial -> Maybe field
getField accessor a b = maybe (accessor a) Just (accessor b)
