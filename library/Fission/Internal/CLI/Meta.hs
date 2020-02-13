module Fission.Internal.CLI.Meta
  ( module Fission.Internal.Meta.Package
  , package
  ) where

import Data.FileEmbed

import Fission.Prelude
import Fission.Internal.Meta.Package hiding (package)

package :: Maybe Package
package =
  case Yaml.decodeEither' $(embedFile "./package.yaml") of
    Left  _err -> Nothing
    Right val  -> Just val
