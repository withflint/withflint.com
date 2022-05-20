module Flint.Jobs where

import Data.Text (Text)
import Data.Text (pack)
import Flint.Types
import Flint.Utils
import Text.Parsec
import Text.Parsec.Text (Parser)
import Control.Monad (void)
import Data.Aeson (ToJSON (..), object, (.=))

instance ToJSON Job where
  toJSON job =
    object [ "url" .= job.url
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
  
  pure ( url
       , Job { .. }
       )
