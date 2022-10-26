module Flint where

import Control.Monad
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified
import Data.Time.Clock (getCurrentTime)
import Flint.Apply
import Flint.Blog
import Flint.Index (index)
import Flint.Jobs
import Flint.Sitemap
import Flint.Types
import Lucid
import Network.HTTP.Types
import Network.Mail.Mime
import Network.Wai
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import System.Directory
import System.Environment (getEnv)
import System.FilePath
import System.FilePath ((</>))
import Text.Parsec.Text
import Text.Parsec.Text (Parser)
import Text.Shakespeare.Text (lt)
import Web.Scotty

routes :: Config -> Static -> ScottyM ()
routes config@(Config {..}) static@(Static {..}) = do
  let unchanged = ["/", "/blog", "/faq", "/join", "/nurse-careers"]

  forM_ unchanged \url -> do
    get url do
      lucid $ index config Nothing

  get "/jobs" do
    redirect "/join"

  get "/jobs/:name" do
    name :: String <- param "name"
    redirect [lt|/join/#{name}|]

  get "/careers" do
    redirect "/join"

  get "/careers/:name" do
    name :: String <- param "name"
    redirect [lt|/join/#{name}|]

  get "/health-care-jobs" do
    redirect "/nurse-careers"

  get "/health-care-jobs/:name" do
    name :: String <- param "name"
    redirect [lt|/nurse-careers/#{name}|]

  get "/healthz" do
    now <- liftIO $ show <$> getCurrentTime

    text [lt|Ok,#{env},#{gitVersion},#{now}|]

  get "/team" do
    redirect "/"

  get "/blog/:article" do
    articleId <- param "article"

    let found = [article.meta | article <- articles, article.slug == articleId]

    lucid $ index
      config
      case found of
        [] -> Nothing
        meta : _ -> Just meta

  get "/articles" do
    json articles

  get "/sitemap.xml" do
    lucidXml $ sitemap articles jobs partnerJobs

  get "/hc" do
    json $ Map.fromList partnerJobs

  get "/j" do
    json $ Map.fromList jobs

  get "/privacy" do
    html privacy

  post "/happly" do
    candidate <- getCandidate

    apply nurseSuccess candidate nurseSuccessEmail

  post "/apply" do
    candidate <- getCandidate

    apply careersEmail candidate careersRenderer

  post "/apply-australia" do
    candidate <- getCandidate

    apply nurseSuccess candidate nurseSuccessEmail

  post "/apply-mexico" do
    candidate <- getCandidate

    apply nurseSuccess candidate nurseSuccessEmailSpanish

  notFound do
    status status200
    lucid $ index config Nothing

run :: Config -> IO ()
run config = do
  articles <- getMarkdown config parseArticle "blog"
  partnerJobs <- getMarkdown config parseJob ("partners")
  jobs <- getMarkdown config parseJob ("jobs")
  privacy <- Text.Lazy.pack <$> readFile (config.root </> "static" </> "privacy.html")

  scotty 5000 do
    let policy = only [("favicon.ico", config.root </> "static" </> "favicon.ico")] <|> addBase config.root
    middleware $ staticPolicy policy
    middleware logStdout
    middleware $ gzip def
    routes config $ Static {..}

dev :: IO ()
dev = do
  let root = "../"
  let gitVersion = "dirty"
  let env = "dev"

  run Config {..}

lucid :: Html () -> ActionM ()
lucid = html . renderText

lucidXml :: Html () -> ActionM ()
lucidXml = text . renderText

getMarkdown :: Config -> Parser a -> FilePath -> IO [a]
getMarkdown (Config {root}) parser subDir = do
  files <- listDirectory $ root </> "content" </> subDir
  let articles = sort $ filter (isExtensionOf "md") files
  reverse . concat . sequence <$> forM articles \filename -> do
    parseFromFile parser $ root </> "content" </> subDir </> filename
