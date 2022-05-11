module Flint.Types (Config (..)) where

import Data.Text (Text)

data Config = Config
  { root :: FilePath
  , gitVersion :: Text
  }
