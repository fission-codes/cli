-- | Whoami command
module Fission.CLI.Command.Whoami (command, whoami) where

import           Fission.Prelude
import           RIO.ByteString

import           Options.Applicative.Simple (addCommand)
import           Servant

import qualified System.Console.ANSI as ANSI

import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.CLI.Environment as Environment
import           Fission.CLI.Config.Types
import           Fission.CLI.Environment.Types

-- | The command to attach to the CLI tree
command :: MonadIO m
        => HasLogFunc cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "whoami"
    "Check the current user"
    (const <| runRIO cfg whoami)
    (pure ())

whoami ::
  ( MonadReader       cfg m
  , MonadIO               m
  , MonadLogger           m
  )
  => m ()
whoami =
  Environment.get >>= \case
    Left err -> do
      logDebug <| displayShow err
      Environment.couldNotRead

    Right env -> do
      let auth = userAuth env
      UTF8.putText "💻 Currently logged in as: "
      liftIO <| ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
      putStr <| basicAuthUsername auth <> "\n"

      liftIO <| ANSI.setSGR [ANSI.Reset]
