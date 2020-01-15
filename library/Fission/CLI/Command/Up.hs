-- | File sync, IPFS-style
module Fission.CLI.Command.Up (command, up) where

import           Fission.Prelude

import           RIO.Directory

import           Options.Applicative.Simple hiding (command)

import           Fission.Internal.Exception

import           Network.IPFS
import qualified Network.IPFS.Add         as IPFS

import           Fission.Web.Client.Auth

import           Fission.CLI.Command.Up.Types as Up
import qualified Fission.CLI.Display.Error    as Error
import qualified Fission.CLI.Prompt.BuildDir  as Prompt
import qualified Fission.CLI.IPFS.Pin         as CLI.Pin
import qualified Fission.CLI.DNS              as CLI.DNS

import           Fission.CLI.Config.Types
import           Fission.CLI.Config.Base
import           Fission.CLI.Config.Connected

import           Fission.CLI.Environment

-- | The command to attach to the CLI tree
command ::
  MonadIO m
  => BaseConfig
  -> CommandM (m ())
command cfg =
  addCommand
    "up"
    "Keep your current working directory up"
    (\options -> void <| runConnected cfg <| up options)
    parseOptions

-- | Sync the current working directory to the server over IPFS
up ::
  ( MonadUnliftIO      m
  , MonadLogger        m
  , MonadLocalIPFS     m
  , MonadEnvironment   m
  , MonadAuthedClient  m
  )
  => Up.Options
  -> m ()
up Up.Options {..} = handleWith_ Error.put' do
  ignoredFiles <- getIgnoredFiles

  toAdd <- Prompt.checkBuildDir path
  absPath <- liftIO <| makeAbsolute toAdd
  logDebug <| "Starting single IPFS add locally of " <> displayShow absPath

  cid     <- liftE <| IPFS.addDir ignoredFiles path

  unless dnsOnly do
    void . liftE <| CLI.Pin.add cid

  liftE <| CLI.DNS.update cid

parseOptions :: Parser Up.Options
parseOptions = do
  dnsOnly <- switch <| mconcat
    [ long "dns-only"
    , help "Only update DNS (skip file sync)"
    ]

  path <- strArgument <| mconcat
    [ metavar "PATH"
    , help    "The file path of the assets or directory to sync"
    , value   "./"
    ]

  return Up.Options {..}
