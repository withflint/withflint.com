module Flint where

import Lucid
import Web.Scotty

import Flint.Types ( Config (..) )
import Flint.Index ( index ) 
import Flint.Blog
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip

type Scotty = ScottyM ()
type Action = ActionM ()

lucid :: Html () -> Action
lucid = html . renderText
      
routes :: Config -> Scotty
routes config = do
  get "/" do
    lucid $ index config

  get "/hc" do
    pure ()

run :: Config -> IO ()
run config = do
  scotty 3000 do
    middleware $ staticPolicy $ addBase config.root 
    middleware logStdout
    middleware $ gzip def
    routes config

testConfig :: Config
testConfig = Config
  { root = "/Users/bramwyllie/Code/Work/withflint.com"
  , gitVersion = "dirty"
  }
  
