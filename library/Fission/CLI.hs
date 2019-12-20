module Fission.CLI (cli) where

import           Fission.Prelude
import           RIO.Process (HasProcessContext)

import           Options.Applicative.Simple

import qualified Fission.Web.Client   as Client

import qualified Network.IPFS.Types   as IPFS

import qualified Fission.CLI.Command.Login         as Login
import qualified Fission.CLI.Command.Logout        as Logout
import qualified Fission.CLI.Command.Register      as Register
import qualified Fission.CLI.Command.ResetPassword as ResetPassword
import qualified Fission.CLI.Command.Up            as Up
import qualified Fission.CLI.Command.Down          as Down
import qualified Fission.CLI.Command.Watch         as Watch
import qualified Fission.CLI.Command.Whoami        as Whoami

-- | Top-level CLI description
cli ::
  ( MonadReader       cfg m
  , MonadLogger           m
  , MonadUnliftIO         m
  , HasLogFunc        cfg
  , HasProcessContext cfg
  , Has Client.Runner cfg
  , Has IPFS.BinPath  cfg
  , Has IPFS.Timeout  cfg
  )
  => m ()
cli = do
  cfg <- ask
  (_, runCLI) <- liftIO <| simpleOptions version description detail (pure ()) do
    Login.command         cfg
    Logout.command        cfg
    Register.command      cfg
    ResetPassword.command cfg
    Up.command            cfg
    Down.command          cfg
    Watch.command         cfg
    Whoami.command        cfg
  runCLI
  where
    version     = "1.23.0"
    description = "CLI to interact with Fission services"
    detail      = mconcat [ "Fission makes developing, deploying, updating "
                          , "and iterating on web applications quick and easy."
                          ]
