module Fission.CLI.Prompt
  ( reask
  , reaskYN
  ) where

import           Fission.Prelude
import           RIO.ByteString as BS hiding (map, pack)
import qualified Fission.Internal.UTF8 as UTF8
import qualified Data.List as L

-- | Continues prompting the user until they put in a valid response
reask :: 
  ( MonadIO m )
  => Text
  -> (ByteString -> Bool)
  -> m ByteString
reask prompt check = do
  UTF8.putText prompt
  resp <- getLine
  if check resp
    then return resp
    else reask prompt check

-- | reask where valid responses are some form of yes/no
reaskYN ::
  ( MonadIO m )
  => Text
  -> m Bool
reaskYN prompt = reask prompt ynTest
  >>= return . isYes

ynTest :: ByteString -> Bool
ynTest resp = isYes resp || isNo resp

isYes :: ByteString -> Bool
isYes resp = L.elem resp (["y", "Y", "yes", "Yes"] :: [ByteString])

isNo :: ByteString -> Bool
isNo resp = L.elem resp (["n", "N", "no", "No"] :: [ByteString])
