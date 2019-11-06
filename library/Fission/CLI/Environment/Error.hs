module Fission.CLI.Environment.Error (Error(..)) where

import RIO

data Error = EnvNotFound
  deriving ( Exception
           , Eq
           , Generic
           )

instance Show Error where
  show EnvNotFound = "Could not find .fission.yaml"
