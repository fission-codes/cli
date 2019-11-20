module Fission.CLI.Environment.Partial.Types (Partial (..)) where

import Fission.Prelude

import           Servant.API

import qualified Fission.IPFS.Types as IPFS
import           Fission.Internal.Orphanage.BasicAuthData ()
import           Fission.Internal.Orphanage.Glob.Pattern ()

data Partial = Partial
  { maybeUserAuth :: Maybe BasicAuthData
  , maybePeers    :: Maybe (NonEmpty IPFS.Peer)
  , maybeIgnored  :: Maybe (IPFS.Ignored)
  } 

instance ToJSON Partial where
  toJSON Partial {..} = object <| catMaybes
    [ ("user_auth" .=) <$> maybeUserAuth
    , ("peers" .=) <$> maybePeers
    , ("ignore" .=) <$> maybeIgnored
    ]

instance FromJSON Partial where
  parseJSON = withObject "Partial" <| \obj ->
    Partial <$> obj .:? "user_auth"
            <*> obj .:? "peers"
            <*> obj .:? "ignore"

instance Semigroup Partial where
  a <> b = Partial
    { maybeUserAuth = getField maybeUserAuth a b
    , maybePeers = getField maybePeers a b
    , maybeIgnored = getField maybeIgnored a b
    }

instance Monoid Partial where
  mempty = Partial
    { maybeUserAuth = Nothing
    , maybePeers = Nothing
    , maybeIgnored = Nothing
    }

getField :: (Partial -> Maybe field) -> Partial -> Partial -> Maybe field
getField accessor a b = maybe (accessor a) Just (accessor b)
