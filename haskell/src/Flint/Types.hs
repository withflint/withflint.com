module Flint.Types (Config (..)) where

data Config = Config
  { root :: FilePath
  , gitVersion :: String 
  }
