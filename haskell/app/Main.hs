module Main where

import Flint (run)
import Flint.Types (Config (..))
import System.Environment (getEnv)

main :: IO ()
main = do
  root <- getEnv "WITHFLINT_ROOT"
  gitVersion <- getEnv "GIT_VERSION"
  env <- getEnv "ENV"

  run Config {..}
