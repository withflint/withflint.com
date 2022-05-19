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
import System.Environment (getEnv)
import Text.Shakespeare.Text (lt)
import Data.Time.Clock (getCurrentTime)
import Data.Text.Lazy qualified as Text.Lazy
import System.FilePath ((</>))

routes :: Config -> Scotty
routes config@(Config { .. }) = do
  get "/jobs" do
    redirect "/join"

  get "/careers" do
    redirect "/join"
  
  get "/healthz" do
    text =<< liftIO do
      env <- getEnv "ENV"
      gitVersion <- getEnv "GIT_VERSION"
      now <- show <$> getCurrentTime
      pure [lt|Ok,#{env},#{gitVersion},#{now}|]
  
  get "/blog/:article" do
    articleId <- param "article"
    articles <- liftIO $ getMarkdown config parseArticle "blog"

    let found = [ article.meta | article <- articles, article.slug == articleId ]
    
    lucid $ index config
      case found of
        []       -> Nothing
        meta : _ -> Just meta

  get "/articles" do
    articles <- liftIO $ getMarkdown config parseArticle "blog"
    json articles

  get "/hc" do
    jobs <- liftIO $ Map.fromList <$> getMarkdown config parseJob "jobs/healthcare"
    json jobs

  get "/j" do
    jobs <- liftIO $ Map.fromList <$> getMarkdown config parseJob "jobs/flint"
    json jobs

  get "/privacy" do
    privacy <- liftIO $ readFile (root </> "static" </> "privacy.html")
    html $ Text.Lazy.pack privacy
  
  post "/happly" do
    candidate <- getCandidate
    apply healthCareEmail candidate healthCareBody

  post "/apply" do
    candidate <- getCandidate
    apply careersEmail candidate careersBody

  notFound do
    lucid $ index config Nothing
 
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
