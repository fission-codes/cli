-- | Continuous file sync
module Fission.CLI.Command.Watch
  ( command
  , handleTreeChanges
  , watcher
  ) where

import           Fission.Prelude
import           RIO.Directory
import           RIO.Process (HasProcessContext)
import qualified RIO.Text as Text

import Servant.Client

import           Options.Applicative.Simple hiding (command)
import           System.FSNotify as FS

import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.Web.Client   as Client
import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.Time         as Time

import           Fission.IPFS.CID.Types
import qualified Fission.IPFS.Types as IPFS
import qualified Fission.AWS.Types  as AWS

import           Fission.Internal.Exception
import           Fission.CLI.Display.Error as CLI.Error

import qualified Fission.CLI.Config.FissionConnected as FissionConnected

import           Fission.CLI.Command.Watch.Types as Watch
import           Fission.CLI.Config.Types
import qualified Fission.CLI.IPFS.Pin            as CLI.Pin
import qualified Fission.CLI.DNS                 as CLI.DNS
import           Fission.CLI.Config.FissionConnected

-- | The command to attach to the CLI tree
command :: MonadIO m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => HasProcessContext cfg
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "watch"
    "Keep your working directory in sync with the IPFS network"
    (\options -> void <| runRIO cfg <| FissionConnected.ensure <| watcher options)
    parseOptions

-- | Continuously sync the current working directory to the server over IPFS
watcher :: MonadRIO             cfg m
        => HasFissionConnected  cfg
        => Watch.Options
        -> m ()
watcher Watch.Options {..} = handleWith_ CLI.Error.put' do
  cfg            <- ask
  absPath        <- makeAbsolute path
  cid@(CID hash) <- liftE <| IPFS.addDir absPath

  UTF8.putText <| "👀 Watching " <> Text.pack absPath <> " for changes...\n"

  when (not dnsOnly) do
    void . liftE <| CLI.Pin.run cid

  liftE <| CLI.DNS.update cid

  liftIO <| FS.withManager \watchMgr -> do
    hashCache <- newMVar hash
    timeCache <- newMVar =<< getCurrentTime
    void <| handleTreeChanges timeCache hashCache watchMgr cfg absPath
    forever <| liftIO <| threadDelay 1000000 -- Sleep main thread

handleTreeChanges :: HasFissionConnected  cfg
                  => MVar UTCTime
                  -> MVar Text
                  -> WatchManager
                  -> cfg
                  -> FilePath
                  -> IO StopListening
handleTreeChanges timeCache hashCache watchMgr cfg dir =
  FS.watchTree watchMgr dir (const True) \_ -> runRIO cfg do
    now     <- getCurrentTime
    oldTime <- readMVar timeCache

    unless (diffUTCTime now oldTime < Time.doherty) do
      void <| swapMVar timeCache now
      threadDelay Time.dohertyMicroSeconds -- Wait for all events to fire in sliding window

      IPFS.addDir dir >>= \case
        Left err ->
          CLI.Error.put' err

        Right cid@(CID newHash) -> do
          oldHash <- swapMVar hashCache newHash
          logDebug <| "CID: " <> display oldHash <> " -> " <> display newHash

          unless (oldHash == newHash) do
            UTF8.putText "\n"
            void <| pinAndUpdateDNS cid

pinAndUpdateDNS :: MonadRIO             cfg m
                => HasFissionConnected  cfg
                => CID
                -> m (Either ClientError AWS.DomainName)
pinAndUpdateDNS cid =
  CLI.Pin.run cid >>= \case
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
