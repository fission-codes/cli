module Fission.CLI.Prompt.Fields
  ( getField
  , getRequired
  , getRequiredSecret
  ) where

import qualified Data.ByteString.UTF8 as UTF8

import           Fission.Prelude
import           RIO.ByteString

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import           Options.Applicative.Simple hiding (command)
import           Servant
import           System.Console.Haskeline

import           Fission.CLI.Config.Types

-- | Prompt a user for a specific value
getField ::
  ( MonadReader       cfg m
  , MonadIO               m
  , MonadLogger           m
  )
  => ByteString
  -> m ByteString
getField fieldName = do
  putStr (fieldName <> ": ")
  getLine


-- | Prompt a user for a value and do not accept an empty value
getRequired ::
  ( MonadReader       cfg m
  , MonadIO               m
  , MonadLogger           m
  )
  => ByteString
  -> m ByteString
getRequired fieldName = do
  fieldValue <- getField fieldName
  if BS.length fieldValue <= 0 then do
    putStr (fieldName <> " is required\n ")
    getRequired fieldName
  else
    return fieldValue

-- | Prompt a user for a secret and do not accept an empty value
getRequiredSecret ::
  ( MonadReader       cfg m
  , MonadIO               m
  , MonadLogger           m
  )
  => ByteString
  -> m ByteString
getRequiredSecret fieldName = do
  let label = UTF8.toString (fieldName <> ": ")
  mayPassword <- liftIO
              <| runInputT defaultSettings
              <| getPassword (Just '•') label

  case mayPassword of
    Nothing -> do
      logError <| show "Unable to read password"
      putStr (fieldName <> " is required\n ") -- TODO: Use error text and emoji
      getRequiredSecret fieldName

    Just password -> do
      let bsPassword = BS.pack password
      if BS.length bsPassword <= 0 then do
        putStr (fieldName <> " is required\n ")
        getRequiredSecret fieldName
      else
        return bsPassword
