module Fission.CLI.Prompt.BuildDir (checkBuildDir) where

import           Fission.Prelude
import           RIO.Directory
import           RIO.FilePath

import           Data.Text as T (pack)

import qualified System.Console.ANSI   as ANSI
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.CLI.Prompt      as Prompt
import qualified Fission.CLI.Environment as Env
import qualified Fission.CLI.Environment.Partial as Env.Partial
import           Fission.CLI.Environment.Partial.Types as Env

checkBuildDir ::
  ( MonadIO m )
  => FilePath
  -> m FilePath
checkBuildDir relPath = do
  absPath <- makeAbsolute relPath
  findEnv absPath >>= \case
    Just (envPath, buildDir) ->
      return <| (takeDirectory envPath) </> buildDir
    Nothing -> guessBuildDir relPath >>= \case
      Nothing -> return relPath 
      Just guess -> do
        buildDir <- promptBuildDir guess >>= \case
          True -> return guess
          False -> return relPath
        let empty = mempty Env.Partial
        let updated = empty { maybeBuildDir = Just buildDir }
        Env.Partial.writeMerge (absPath </> ".fission.yaml") updated
        return buildDir

findEnv :: ( MonadIO m ) => FilePath -> m (Maybe (FilePath, FilePath))
findEnv path = Env.findRecurse (isJust . maybeBuildDir) path >>= \case
  Nothing -> return Nothing
  Just (envPath, env) -> do
    case maybeBuildDir env of
      Nothing -> return Nothing
      Just buildDir -> return <| Just (envPath, buildDir)


promptBuildDir :: ( MonadIO m ) => FilePath -> m Bool
promptBuildDir path = do
  UTF8.putText <| "👷 We found a possible build dir: "
  liftIO <| ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  UTF8.putTextLn <| T.pack path
  liftIO <| ANSI.setSGR [ANSI.Reset]
  -- \xD83E\xDD14 == 🤔 (will be available in the next version of the parser)
  Prompt.reaskYN "\xD83E\xDD14 Would you like to upload that instead of the project root? (y/n): "

guessBuildDir :: ( MonadIO m ) => FilePath -> m (Maybe FilePath)
guessBuildDir path = foldM (foldGuess path) Nothing (buildGuesses path)

foldGuess :: MonadIO m => FilePath -> Maybe FilePath -> FilePath -> m (Maybe FilePath)
foldGuess base acc path = (doesDirectoryExist <| combine base path) >>= \case
  True -> return <| Just path
  False -> return acc

buildGuesses :: FilePath -> [FilePath]
buildGuesses base = map (combine base)
  [ "_site" -- jekyll, hakyll, eleventy
  , "site" -- forgot which
  , "public" -- gatsby, hugo
  , "dist" -- nuxt
  , "output" -- pelican
  , "out" -- hexo
  , "build" -- create-react-app, metalsmith, middleman
  , "website/build" -- docusaurus
  , "docs" -- many others
  ]
