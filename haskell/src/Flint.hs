module Flint where

import Lucid
import Web.Scotty

import Flint.Types
import Flint.Index (index) 
import Flint.Blog
import Flint.Jobs
import Flint.Apply
import Flint.Sitemap
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
import Data.Text (Text)
import Data.Text.Lazy qualified as Text.Lazy
import System.FilePath ((</>))

routes :: Config -> Static -> Scotty
routes config@(Config { .. }) static@(Static { .. }) = do
  let unchanged = [ "/", "/blog", "/faq", "/team", "/join", "/nurse-careers" ]

  forM_ unchanged \url -> do
    get url do
      lucid $ index config Nothing
  
  get "/jobs" do
    redirect "/join"

  get "/careers" do
    redirect "/join"
  
  get "/healthz" do
    now <- liftIO $ show <$> getCurrentTime

    text [lt|Ok,#{env},#{gitVersion},#{now}|]
  
  get "/blog/:article" do
    articleId <- param "article"

    let found = [ article.meta | article <- articles, article.slug == articleId ]
    
    lucid $ index config
      case found of
        []       -> Nothing
        meta : _ -> Just meta

  get "/articles" do
    json articles

  get "/sitemap.xml" do
    lucidXml $ sitemap articles
  
  get "/hc" do
    json healthCareJobs

  get "/j" do
    json flintJobs

  get "/privacy" do
    html privacy
  
  post "/happly" do
    candidate <- getCandidate
    
    apply healthCareEmail candidate healthCareRenderer 

  post "/apply" do
    candidate <- getCandidate
    
    apply careersEmail candidate careersRenderer

  notFound do
    lucid $ index config Nothing

run :: Config -> IO ()
run config = do
  articles <- getMarkdown config parseArticle "blog"
  healthCareJobs <- Map.fromList <$> getMarkdown config parseJob ("jobs" </> "healthcare")
  flintJobs <- Map.fromList <$> getMarkdown config parseJob ("jobs" </> "flint")
  privacy <- Text.Lazy.pack <$> readFile (config.root </> "static" </> "privacy.html")
  
  scotty 5000 do
    let policy = only [("favicon.ico", config.root </> "static" </> "favicon.ico")] <|> addBase config.root
    middleware $ staticPolicy policy 
    middleware logStdout
    middleware $ gzip def
    routes config $ Static { .. }

testConfig :: Config
testConfig = Config
  { root = "/Volumes/CS/code/work/withflint.com"
  , gitVersion = "dirty"
  , env = "dev"
  }
