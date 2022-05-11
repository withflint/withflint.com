module Flint.Blog where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Function
import Lucid
import Lucid.Base
import Text.Parsec
import Text.Parsec.Text
import Data.Time
import Control.Monad

data Meta = Meta
  { type_ :: Text
  , title :: Text
  , url :: Text
  , image :: Text
  , description :: Text
  , author :: Text
  , publishedTime :: Text
  } deriving (Show, Eq)

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

eol :: String
eol = "\n\r"

line :: Parser Text
line = do
  lines <- many $ noneOf eol
  oneOf eol
  pure $ Text.pack lines

separator :: Parser ()
separator = void do
  string "---"
  endOfLine
  
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
