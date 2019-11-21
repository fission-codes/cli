module Fission.CLI.Prompt.BuildDir
  ( checkBuildDir
  , findBuildDir
  ) where

import           Fission.Prelude
import           RIO.Directory
import           RIO.FilePath

import           Data.Text as T (pack)
import qualified System.Console.ANSI as ANSI
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.CLI.Prompt           as Prompt

checkBuildDir ::
  ( MonadIO m)
  => FilePath
  -> m (FilePath)
checkBuildDir path = findBuildDir path >>= \case
  Nothing -> return path 
  Just dir -> do
    UTF8.putText <| "👷 We found a possible build dir: "
    liftIO <| ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
    UTF8.putTextLn <| T.pack dir
    liftIO <| ANSI.setSGR [ANSI.Reset]
    resp <- Prompt.reaskYN "❔ Would you like to upload that instead of the project root? (y/n): "
    case resp of 
      False -> return path
      True -> return dir

findBuildDir ::
  ( MonadIO m )
  => FilePath
  -> m (Maybe FilePath)
findBuildDir path = foldM (foldGuess path) Nothing (buildGuesses path)

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
