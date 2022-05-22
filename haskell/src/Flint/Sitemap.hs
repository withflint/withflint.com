module Flint.Sitemap where

import Lucid.Base
import Data.Text (Text)
import Control.Monad (forM_)
import Flint.Types (Article (..))
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Html.Utf8 qualified
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

genArticles :: [Article] -> Html ()
genArticles articles = do
  forM_ articles \(Article { .. }) -> do
    url_ $ loc_ $ toHtml [st|https://withflint.com/blog/#{slug}|]

sitemap :: [Article] -> Html ()
sitemap articles = withXml do
  urlset_ [ xmlns_ "http://www.sitemaps.org/schemas/sitemap/0.9" ] do
    url_ $ loc_ "https://withflint.com/"
    url_ $ loc_ "https://withflint.com/nurse-careers"
    url_ $ loc_ "https://withflint.com/contact"
    url_ $ loc_ "https://withflint.com/blog"
    genArticles articles
    url_ $ loc_ "https://withflint.com/join"
    url_ $ loc_ "https://withflint.com/internationally-educated-nurses-faq"
