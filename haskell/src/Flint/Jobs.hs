module Flint.Jobs where

import Control.Monad (void)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text, pack)
import Flint.Types
import Text.Parsec
import Text.Parsec.Text (Parser)

import Control.Monad
import Control.Monad (void)
import Data.ByteString.Lazy (ByteString)
import Data.List
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified
import Data.Text.Lazy.Encoding qualified
import Flint.Types
import Lucid.Base (Attribute, Html, ToHtml (toHtmlRaw), makeAttribute, renderText)
import Network.Mail.Mime
import Network.Mail.SMTP
import Network.Wai
import Network.Wai.Parse (FileInfo (..))
import System.Directory
import System.FilePath
import Text.Parsec (manyTill, noneOf, oneOf, string)
import Text.Parsec.Text
import Text.Parsec.Text (Parser)
import Text.Shakespeare.Text (lt)
import Web.Scotty

instance ToJSON Job where
  toJSON job =
    object
      [ "url" .= job.url
      , "title" .= job.title
      , "location" .= job.location
      , "equity" .= job.equity
      , "experience" .= job.experience
      , "description" .= job.description
      ]

parseJob :: Parser (Text, Job)
parseJob = do
  url <- line
  title <- line
  location <- line
  equity <- line
  experience <- line

  void line

  description <- pack <$> many anyChar

  pure
    ( url
    , Job {..}
    )

eol :: [Char]
eol =
  [ '\n'
  , '\r'
  ]

line :: Parser Text
line =
  pack <$> manyTill (noneOf eol) (oneOf eol)
