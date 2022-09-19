module Flint.Sitemap where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Html.Utf8 qualified
import Control.Monad (forM_)
import Data.Text (Text)
import Flint.Types (Article (..), Job (..))
import Lucid.Base
import Text.Shakespeare.Text

urlset_ :: Term arg result => arg -> result
urlset_ = term "urlset"

xmlns_ :: Text -> Attribute
xmlns_ = makeAttribute "xmlns"

url_ :: Term arg result => arg -> result
url_ = term "url"

loc_ :: Term arg result => arg -> result
loc_ = term "loc"

withXml :: Html a -> Html a
withXml body = do
  toHtmlRaw "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  body

sitemap :: [Article] -> [(Text, Job)] -> [(Text, Job)] -> Html ()
sitemap articles jobs partnerJobs = withXml do
  urlset_ [xmlns_ "http://www.sitemaps.org/schemas/sitemap/0.9"] do
    url_ $ loc_ "https://withflint.com/"
    url_ $ loc_ "https://withflint.com/partnerships"
    url_ $ loc_ "https://withflint.com/nurse-careers"
    genJobs "nurse-careers" partnerJobs
    url_ $ loc_ "https://withflint.com/contact"
    url_ $ loc_ "https://withflint.com/blog"
    genArticles articles
    url_ $ loc_ "https://withflint.com/join"
    genJobs "join" jobs
    url_ $ loc_ "https://withflint.com/internationally-educated-nurses-faq"

genArticles :: [Article] -> Html ()
genArticles articles = do
  forM_ articles \(Article {..}) -> do
    url_ $ loc_ $ toHtml [st|https://withflint.com/blog/#{slug}|]

genJobs :: Text -> [(Text, Job)] -> Html ()
genJobs path jobs = do
  forM_ jobs \((url, _)) -> do
    url_ $ loc_ $ toHtml [st|https://withflint.com/#{path}/#{url}|]
