-- | Guards to ensure we have the appropriate data available to run a protected action
module Fission.CLI.Config.LoggedIn
  ( ensure
  , module Fission.CLI.Config.LoggedIn.Types
  ) where

import           RIO

import           Data.Has

import           Fission.Internal.Constraint

import qualified Fission.Web.Client   as Client

import qualified Fission.Config as Config
import           Fission.CLI.Config.LoggedIn.Types
import           Fission.CLI.Config.LoggedIn.Error.Types

import           Fission.CLI.Environment.Types as Environment
import qualified Fission.CLI.Environment       as Environment

-- | Ensure we have a local config file with the appropriate data
--
-- Takes a @LoggedIn@-dependant action, and lifts it into an environment that
-- contains a superset of the environment
ensure
  :: ( MonadRIO          cfg m
     , HasLogFunc        cfg
     , Has Client.Runner cfg
     )
  => RIO LoggedIn a
  -> m (Either Error a)
ensure action = do
  _logFunc     :: LogFunc        <- view logFuncL
  _fissionAPI  :: Client.Runner  <- Config.get

  -- Get our stored user config
  Environment.get >>= \case
    Right config -> do
      let
        _userAuth = Environment.userAuth config
      -- All setup and logged in!
      result <- liftIO $ runRIO LoggedIn {..} action
      Right <$> return result

    Left err -> do
      -- We were unable to read the users config
      logDebug $ displayShow err
      Environment.couldNotRead
      return $ Left NotLoggedIn
