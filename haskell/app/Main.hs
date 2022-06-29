module Main where

import System.Environment (getEnv)
import Flint (run)
import Flint.Types (Config (..))

main :: IO ()
main = do
  root <- getEnv "WITHFLINT_ROOT"
  gitVersion <- getEnv "GIT_VERSION"
  env <- getEnv "ENV"

  run Config { .. }
