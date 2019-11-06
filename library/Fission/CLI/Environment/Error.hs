module Fission.CLI.Environment.Error (Error(..)) where

import RIO

data Error = NoEnv
  deriving ( Exception
           , Eq
           , Generic
           )

instance Show Error where
  show NoEnv = "Could not find .fission.yaml"
