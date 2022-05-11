module Flint.Blog where

import Data.Text
import Data.Function
import Lucid
import Lucid.Base
import Text.Parsec
import Text.Parsec.Text
import Data.Time

data Meta = Meta
  { type_ :: Text
  , title :: Text
  , url :: Text
  , image :: Text
  , description :: Text
  , author :: Text
  , publishedTime :: Text
  }

data Article = Article
  { author :: Text
  , bio :: Text
  , link :: Text
  , avatar :: Text
  , slug :: Text
  , date :: Text
  , title :: Text
  , sub :: Text
  , body :: Text
  , meta :: Meta
  }

property_ :: Text -> Attribute
property_ = makeAttribute "property"

generateMeta :: Meta -> Html ()
generateMeta metadata = do
  meta_ [ property_ "og:type", content_ metadata.type_ ]
  meta_ [ property_ "og:title", content_ metadata.title ]
  meta_ [ property_ "og:url", content_ metadata.url ]
  meta_ [ property_ "og:image", content_ metadata.image ]
  meta_ [ property_ "og:description", content_ metadata.description ]
  meta_ [ property_ "article:author", content_ metadata.author ]
  meta_ [ property_ "article:publishedTime", content_ metadata.publishedTime ]


parseArticle :: [Text] -> Maybe Article
parseArticle (author : bio : link : avatar : _ : slug : date : title : sub : _ : body) =
  Just Article { body = Data.Text.unlines body
               , meta = Meta { type_ = "article"
                             , title = title
                             , url = "https://withflint.com/blog/" <> slug
                             , image = "https://withflint.com/static/images/blog/" <> slug <> ".jpeg"
                             , description = sub
                             , author = link
                             , publishedTime = date
                             }
               , ..
               }
parseArticle _ = Nothing
