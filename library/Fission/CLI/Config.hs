module Fission.CLI.Config (runLocal) where

import           RIO

import           Fission.Internal.Constraint

runLocal :: MonadRIO oldCfg m => newCfg -> RIO newCfg a -> m a
runLocal newCfg action = liftIO $ runRIO newCfg action
