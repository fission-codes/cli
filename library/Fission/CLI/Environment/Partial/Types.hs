module Fission.CLI.Environment.Partial.Types (Partial (..)) where

import Fission.Prelude

import qualified Network.IPFS.Types as IPFS
import           Fission.Internal.Orphanage.Glob.Pattern ()

data Partial = Partial
  { maybePeers    :: Maybe (NonEmpty IPFS.Peer)
  , maybeIgnored  :: Maybe (IPFS.Ignored)
  , maybeBuildDir :: Maybe (FilePath)
  }

instance ToJSON Partial where
  toJSON Partial {..} = object <| catMaybes
    [ ("peers" .=)     <$> maybePeers
    , ("ignore" .=)    <$> maybeIgnored
    , ("build_dir" .=) <$> maybeBuildDir
    ]

instance FromJSON Partial where
  parseJSON = withObject "Partial" <| \obj ->
    Partial <$> obj .:? "peers"
            <*> obj .:? "ignore"
            <*> obj .:? "build_dir"

instance Semigroup Partial where
  a <> b = Partial
    { maybePeers    = getField maybePeers a b
    , maybeIgnored  = getField maybeIgnored a b
    , maybeBuildDir = getField maybeBuildDir a b
    }

instance Monoid Partial where
  mempty = Partial
    { maybePeers = Nothing
    , maybeIgnored = Nothing
    , maybeBuildDir = Nothing
    }

getField :: (Partial -> Maybe field) -> Partial -> Partial -> Maybe field
getField accessor a b = maybe (accessor a) Just (accessor b)

