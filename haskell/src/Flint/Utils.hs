module Flint.Utils where

import Lucid.Base (Attribute, Html, renderText, makeAttribute)
import Data.Text (Text, pack)
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

type Attachment = (Text, Text, ByteString)

fileToAttachment :: File -> Attachment
fileToAttachment (content, info) =
  ( Data.Text.Encoding.decodeUtf8 info.fileContentType
  , Data.Text.Encoding.decodeUtf8 info.fileName
  , Data.Text.Lazy.Encoding.encodeUtf8 content
  )
