module Fission.CLI (cli) where

import           Fission.Prelude

import           Options.Applicative.Simple

import           Fission.CLI.Config.Base

import qualified Fission.CLI.Command.Setup         as Setup
import qualified Fission.CLI.Command.Up            as Up
import qualified Fission.CLI.Command.Down          as Down
import qualified Fission.CLI.Command.Watch         as Watch
import qualified Fission.CLI.Command.Whoami        as Whoami

-- | Top-level CLI description
cli ::
  MonadUnliftIO m
  => BaseConfig
  -> m ()
cli cfg = do
  (_, runCLI) <- liftIO <| simpleOptions version description detail (pure ()) do
    Setup.command         cfg
    Up.command            cfg
    Down.command          cfg
    Watch.command         cfg
    Whoami.command        cfg
  runCLI
  where
    version     = "1.23.1"
    description = "CLI to interact with Fission services"
    detail      = mconcat [ "Fission makes developing, deploying, updating "
                          , "and iterating on web applications quick and easy."
                          ]
