module Flint.Blog where

import Control.Monad
import Control.Monad (void)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.List
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified
import Data.Text.Lazy.Encoding qualified
import Flint.Types
import Lucid
import Lucid.Base (Attribute, Html, ToHtml (toHtmlRaw), makeAttribute, renderText)
import Network.Mail.Mime
import Network.Mail.SMTP
import Network.Wai
import Network.Wai.Parse (FileInfo (..))
import System.Directory
import System.FilePath
import Text.Parsec
import Text.Parsec (manyTill, noneOf, oneOf, string)
import Text.Parsec.Text
import Text.Parsec.Text (Parser)
import Text.Shakespeare.Text (lt)
import Web.Scotty

instance ToJSON Meta where
  toJSON meta =
    object
      [ "type" .= meta.type_
      , "title" .= meta.title
      , "url" .= meta.url
      , "image" .= meta.image
      , "description" .= meta.description
      , "author" .= meta.author
      , "publishedTime" .= meta.publishedTime
      ]

instance ToJSON Article where
  toJSON article =
    object
      [ "author" .= article.author
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
  meta_ [property_ "og:type", content_ metadata.type_]
  meta_ [property_ "og:title", content_ metadata.title]
  meta_ [property_ "og:url", content_ metadata.url]
  meta_ [property_ "og:image", content_ metadata.image]
  meta_ [property_ "og:description", content_ metadata.description]
  meta_ [property_ "article:author", content_ metadata.author]
  meta_ [property_ "article:publishedTime", content_ metadata.publishedTime]

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

  let meta =
        Meta
          { type_ = "article"
          , title = title
          , url = "https://withflint.com/blog/" <> slug
          , image = "https://withflint.com/static/images/blog/" <> slug <> ".jpeg"
          , description = sub
          , author = link
          , publishedTime = date
          }

  pure Article {..}

separator :: Parser ()
separator = void do
  string "---"
  oneOf eol

property_ :: Text -> Attribute
property_ = makeAttribute "property"

eol :: [Char]
eol =
  [ '\n'
  , '\r'
  ]

line :: Parser Text
line =
  pack <$> manyTill (noneOf eol) (oneOf eol)
