module Main where

import System.Environment
import Flint.Types (Config (..))
import Flint (run)

main :: IO ()
main = do
  root <- getEnv "WITHFLINT_ROOT"
  gitVersion <- getEnv "GIT_VERSION"
  env <- getEnv "ENV"
  
  run Config { .. }
