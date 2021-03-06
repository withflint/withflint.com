module Flint.Utils where

import Lucid.Base (Attribute, Html, renderText, makeAttribute, ToHtml (toHtmlRaw))
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Control.Monad (void)
import Text.Parsec (manyTill, noneOf, oneOf, string)
import Text.Parsec.Text (Parser)
import Flint.Types
import System.Directory
import System.FilePath
import Control.Monad
import Data.List
import Text.Parsec.Text
import Web.Scotty
import Network.Mail.Mime
import Network.Mail.SMTP
import Network.Wai
import Network.Wai.Parse (FileInfo (..))
import Data.ByteString.Lazy (ByteString)
import Data.Text.Encoding qualified
import Data.Text.Lazy.Encoding qualified
import Text.Shakespeare.Text (lt)

type Scotty = ScottyM ()
type Action = ActionM ()

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

separator :: Parser ()
separator = void do
  string "---"
  oneOf eol
  
getMarkdown :: Config -> Parser a -> FilePath -> IO [a]
getMarkdown (Config { root }) parser subDir = do
  files <- listDirectory $ root </> "content" </> subDir
  let articles = sort $ filter (isExtensionOf "md") files
  reverse . concat . sequence <$> forM articles \filename -> do
    parseFromFile parser $ root </> "content" </> subDir </> filename

lucid :: Html () -> Action
lucid = html . renderText

lucidXml :: Html () -> Action
lucidXml = text . renderText

comment_ :: Text -> Html ()
comment_ body = do
  toHtmlRaw [lt|<!-- #{body} -->|]

type Attachment = (Text, Text, ByteString)

fileToAttachment :: File -> Attachment
fileToAttachment (fieldName, info) =
  ( Data.Text.Encoding.decodeUtf8 $ fileContentType info
  , Data.Text.Encoding.decodeUtf8 $ fileName info
  , fileContent info
  )
