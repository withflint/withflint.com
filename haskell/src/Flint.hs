module Flint where

import Lucid
import Web.Scotty

import Flint.Types (Config (..))
import Flint.Index (index) 
import Flint.Blog
import Flint.Jobs
import Flint.Apply
import Flint.Utils
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Data.Map qualified as Map
import Data.Text.Lazy.Encoding qualified
import Network.Mail.Mime

routes :: Config -> Scotty
routes config = do
  let unchanged = [ "/", "/blog", "/privacy", "/faq", "/careers", "/team", "/join", "/nurse-careers" ]

  forM_ unchanged \url -> do
    get url do
      lucid $ index config Nothing
   
  get "/blog/:article" do
    articleId <- param "article"
    articles <- liftIO $ getMarkdown config parseArticle "blog"
    let meta = head [ article.meta | article <- articles, article.slug == articleId ]
    lucid $ index config $ Just meta

  get "/articles" do
    articles <- liftIO $ getMarkdown config parseArticle "blog"
    json articles

  get "/hc" do
    jobs <- liftIO $ Map.fromList <$> getMarkdown config parseJob "jobs/healthcare"
    json jobs

  get "/j" do
    jobs <- liftIO $ Map.fromList <$> getMarkdown config parseJob "jobs/flint"
    json jobs
{-
  get "/mail" do
    mail <- apply sampleLocation sampleCandidate
    rendered <- liftIO $ renderMail' mail
    text $ Data.Text.Lazy.Encoding.decodeUtf8 rendered
 -}
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
