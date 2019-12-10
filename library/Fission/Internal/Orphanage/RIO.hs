{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.Orphanage.RIO () where

import Fission.Prelude

import RIO.Orphans ()

import qualified RIO.ByteString.Lazy as Lazy

import qualified Fission.Config        as Config

import           Network.IPFS
import           Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Process

instance 
  ( HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => MonadLocalIPFS (RIO cfg) where
    runLocal opts arg = do
      IPFS.BinPath ipfs <- Config.get
      IPFS.Timeout secs <- Config.get
      let opts' = ("--timeout=" <> show secs <> "s") : opts

      runProc readProcess ipfs (byteStringInput arg) byteStringOutput opts' >>= \case
        (ExitSuccess, contents, _) ->
          return <| Right contents
        (ExitFailure _, _, stdErr)
          | Lazy.isSuffixOf "context deadline exceeded" stdErr -> 
              return . Left <| Process.Timeout secs
          | otherwise ->
            return . Left <| Process.UnknownErr stdErr
