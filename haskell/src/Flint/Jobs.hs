module Flint.Jobs where

import Data.Text (Text)
import Data.Text qualified as Text

data Job = Job
  { url :: Text
  , title :: Text
  , location :: Text
  , equity :: Text
  , experience :: Text
  , description :: Text
  }


parseJob :: [Text] -> Maybe (Text, Job)
parseJob lines =
  case lines of
    url : title : location : equity : experience : _ : body ->
      Just (url, Job { description = Text.unlines body, .. }) 
    _ ->
      Nothing
