module Flint.Blog where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Function
import Lucid
import Lucid.Base
import Text.Parsec
import Text.Parsec.Text
import Data.Time
import Control.Monad
import System.Directory
import Flint.Types
import Flint.Utils
import System.FilePath.Posix
import Data.List (sort)
import Data.Aeson (ToJSON (..), object, (.=))

data Meta = Meta
  { type_ :: Text
  , title :: Text
  , url :: Text
  , image :: Text
  , description :: Text
  , author :: Text
  , publishedTime :: Text
  } deriving (Show, Eq)

instance ToJSON Meta where
  toJSON meta =
    object [ "type" .= meta.type_
           , "title" .= meta.title
           , "url" .= meta.url
           , "image" .= meta.image
           , "description" .= meta.description
           , "author" .= meta.author
           , "publishedTime" .= meta.publishedTime
           ]

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
  } deriving (Show, Eq)

instance ToJSON Article where
  toJSON article =
    object [ "author" .= article.author
           , "bio" .= article.bio
           , "link" .= article.link
           , "avatar" .= article.avatar
           , "slug" .= article.slug
           , "date" .= article.date
           , "title" .= article.title
           , "sub" .= article.sub
           , "body" .= article.body
           , "meta" .= article.meta
           ]

generateMeta :: Meta -> Html ()
generateMeta metadata = do
  meta_ [ property_ "og:type", content_ metadata.type_ ]
  meta_ [ property_ "og:title", content_ metadata.title ]
  meta_ [ property_ "og:url", content_ metadata.url ]
  meta_ [ property_ "og:image", content_ metadata.image ]
  meta_ [ property_ "og:description", content_ metadata.description ]
  meta_ [ property_ "article:author", content_ metadata.author ]
  meta_ [ property_ "article:publishedTime", content_ metadata.publishedTime ]

parseArticle :: Parser Article
parseArticle = do
  author <- line
  bio <- line
  link <- line
  avatar <- line

  separator

  slug <- line
  date <- line
  title <- line
  sub <- line

  separator
  
  body <- Text.pack <$> many anyChar

  let meta = Meta
        { type_ = "article"
        , title = title
        , url = "https://withflint.com/blog/" <> slug
        , image = "https://withflint.com/static/images/blog/" <> slug <> ".jpeg"
        , description = sub
        , author = link
        , publishedTime = date
        }

  pure Article { .. }
