module Flint.Utils where

import Lucid.Base (Attribute, makeAttribute)
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
  
getMarkdownWithMeta :: Config -> Parser a -> FilePath -> IO [a]
getMarkdownWithMeta Config { root } parser subDir = do
  files <- listDirectory (root <> subDir)
  let articles = sort $ filter (isExtensionOf "md") files
  concat . sequence <$> forM articles \filename -> do
    parseFromFile parser $ root <> subDir <> filename
