module Fission.CLI.Prompt.Fields
  ( getField
  , getRequired
  , getRequiredSecret
  ) where

import           Fission.Prelude

import           RIO.ByteString
import qualified Fission.Internal.UTF8 as UTF8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as BS

import           System.Console.Haskeline

import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Prompt.Error.Types as Prompt.Error

-- | Prompt a user for a specific value
getField :: MonadIO m => ByteString -> m ByteString
getField fieldName = do
  putStr (fieldName <> ": ")
  getLine

-- | Prompt a user for a value and do not accept an empty value
getRequired :: (MonadIO m, MonadLogger m) => ByteString -> m ByteString
getRequired fieldName = do
  fieldValue <- getField fieldName
  if BS.length fieldValue <= 0
    then do
      showRequiredError fieldName
      getRequired fieldName
    else
      return fieldValue

-- | Prompt a user for a secret and do not accept an empty value
getRequiredSecret :: (MonadIO m, MonadLogger m) => ByteString -> m ByteString
getRequiredSecret fieldName =
  (fieldName <> ": ")
    |> UTF8.toString
    |> getPassword (Just '•')
    |> runInputT defaultSettings
    |> liftIO
    |> bind \case
      Nothing -> do
        logErrorN "Unable to read password"
        showRequiredError fieldName
        getRequiredSecret fieldName

      Just "" -> do
        showRequiredError fieldName
        getRequiredSecret fieldName

      Just password ->
        return <| BS.pack password

showRequiredError :: (MonadIO m, MonadLogger m) => ByteString -> m ()
showRequiredError fieldName = CLI.Error.put Prompt.Error.RequiredField ((UTF8.textShow fieldName) <> " is required")
