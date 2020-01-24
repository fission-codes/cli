-- | Continuous file sync
module Fission.CLI.Command.Watch
  ( command
  , handleTreeChanges
  , watcher
  ) where

import           Fission.Prelude
import           RIO.Directory
import qualified RIO.Text as Text
import           Data.Function

import Servant.Client

import           Options.Applicative.Simple hiding (command)
import           System.FSNotify as FS

import qualified Fission.Internal.UTF8 as UTF8

import           Fission.Web.Client as Client
import qualified Fission.Time       as Time

import           Network.IPFS
import qualified Network.IPFS.Add as IPFS
import           Network.IPFS.CID.Types

import qualified Fission.URL.DomainName.Types as URL

import           Fission.Internal.Exception
import           Fission.CLI.Display.Error as CLI.Error

import           Fission.CLI.Config.Base
import           Fission.CLI.Config.Connected

import           Fission.CLI.Command.Watch.Types as Watch
import qualified Fission.CLI.Prompt.BuildDir  as Prompt
import           Fission.CLI.Config.Types
import qualified Fission.CLI.IPFS.Pin            as CLI.Pin
import qualified Fission.CLI.DNS                 as CLI.DNS

import           Fission.CLI.Environment

-- | The command to attach to the CLI tree
command :: MonadIO m => BaseConfig -> CommandM (m ())
command cfg =
  addCommand
    "watch"
    "Keep your working directory in sync with the IPFS network"
    (\options -> void <| runConnected cfg <| watcher options cfg)
    parseOptions

-- | Continuously sync the current working directory to the server over IPFS
watcher ::
  ( MonadUnliftIO    m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadWebClient   m
  )
  => Watch.Options
  -> BaseConfig
  -> m ()
watcher Watch.Options {..} cfg = handleWith_ CLI.Error.put' do
  ignoredFiles <- getIgnoredFiles

  toAdd <- Prompt.checkBuildDir path
  absPath        <- makeAbsolute toAdd
  logDebug <| "Starting single IPFS add locally of " <> displayShow absPath

  cid@(CID hash) <- liftE <| IPFS.addDir ignoredFiles absPath

  UTF8.putText <| "👀 Watching " <> Text.pack absPath <> " for changes...\n"

  when (not dnsOnly) do
    void . liftE <| CLI.Pin.add cid

  liftE <| CLI.DNS.update cid

  cfg' <- liftE <| liftConfig cfg

  liftIO <| FS.withManager \watchMgr -> do
    hashCache <- newMVar hash
    timeCache <- newMVar =<< getCurrentTime
    void <| handleTreeChanges timeCache hashCache watchMgr cfg' absPath
    forever <| liftIO <| threadDelay 1000000 -- Sleep main thread

handleTreeChanges ::
     MVar UTCTime
  -> MVar Text
  -> WatchManager
  -> ConnectedConfig
  -> FilePath
  -> IO StopListening
handleTreeChanges timeCache hashCache watchMgr cfg dir =
  FS.watchTree watchMgr dir (const True) \_ -> runConnected' cfg do
    now     <- getCurrentTime
    oldTime <- readMVar timeCache

    unless (diffUTCTime now oldTime < Time.doherty) do
      void <| swapMVar timeCache now
      threadDelay Time.dohertyMicroSeconds -- Wait for all events to fire in sliding window

      IPFS.addDir [] dir >>= \case
        Left err ->
          CLI.Error.put' err

        Right cid@(CID newHash) -> do
          oldHash <- swapMVar hashCache newHash
          logDebug <| "CID: " <> display oldHash <> " -> " <> display newHash

          unless (oldHash == newHash) do
            UTF8.putText "\n"
            void <| pinAndUpdateDNS cid

pinAndUpdateDNS ::
  ( MonadUnliftIO  m
  , MonadWebClient m
  , MonadLogger    m
  )
  => CID
  -> m (Either ClientError URL.DomainName)
pinAndUpdateDNS cid =
  CLI.Pin.add cid >>= \case
    Left err -> do
      logError <| displayShow err
      return <| Left err

    Right _ ->
      CLI.DNS.update cid

parseOptions :: Parser Watch.Options
parseOptions = do
  dnsOnly <- switch <| mconcat
    [ long "dns-only"
    , help "Only update DNS (i.e. don't actively sync files to the server)"
    ]

  path <- strArgument <| mconcat
    [ metavar "PATH"
    , help    "The file path of the assets or directory to watch"
    , value   "./"
    ]

  return Watch.Options {..}
