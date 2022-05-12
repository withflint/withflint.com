module Flint where

import Lucid
import Web.Scotty

import Flint.Types (Config (..))
import Flint.Index (index) 
import Flint.Blog
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Control.Monad.IO.Class
import Web.Scotty.Internal.Types qualified

type Scotty = ScottyM ()
type Action = ActionM ()

lucid :: Html () -> Action
lucid = html . renderText

routes :: Config -> Scotty
routes config = do
  get "/" do
    lucid $ index config Nothing

  get "/hc" do
    pure ()

  get "/blog" do
    lucid $ index config Nothing
    
  get "/blog/:article" do
    articleId <- param "article"
    articles <- liftIO $ getArticles config
    let meta = head [article.meta | article <- articles, article.slug == articleId]
    lucid $ index config $ Just meta
    
run :: Config -> IO ()
run config = do
  scotty 5000 do
    middleware $ staticPolicy $ addBase config.root 
    middleware logStdout
    middleware $ gzip def
    routes config

testConfig :: Config
testConfig = Config
  { root = "/Volumes/CS/code/work/withflint.com"
  , gitVersion = "dirty"
  }
  
