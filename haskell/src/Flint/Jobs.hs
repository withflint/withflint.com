module Flint.Jobs where

import Data.Text (Text)
import Data.Text (pack)
import Flint.Utils
import Text.Parsec
import Text.Parsec.Text (Parser)
import Control.Monad (void)

data Job = Job
  { url :: Text
  , title :: Text
  , location :: Text
  , equity :: Text
  , experience :: Text
  , description :: Text
  }

parseJob :: Parser (Text, Job)
parseJob = do
  url <- line
  title <- line
  location <- line
  equity <- line
  experience <- line
  
  void line

  description <- pack <$> many anyChar
  
  pure ( url
       , Job { .. }
       )
