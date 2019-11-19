module Fission.CLI.Environment.Error (Env(..)) where

import RIO

data Env = EnvNotFound | EnvIncomplete
  deriving ( Exception
           , Eq
           , Generic
           )

instance Show Env where
  show EnvNotFound = "Could not find .fission.yaml"
  show EnvIncomplete = "Could not construct a full .fission.yaml"
