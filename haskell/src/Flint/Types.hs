module Flint.Types where

import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Network.Mail.Mime (Address)
import Data.Map (Map)
import Lucid (Html)

data Config = Config
  { root :: FilePath
  , gitVersion :: String
  , env :: String
  } deriving Show

data Static = Static
  { articles :: [Article]
  , healthCareJobs :: Map Text.Text Job
  , flintJobs :: Map Text.Text Job
  , privacy :: Text.Lazy.Text
  } deriving Show

data Meta = Meta
  { type_ :: Text.Text
  , title :: Text.Text
  , url :: Text.Text
  , image :: Text.Text
  , description :: Text.Text
  , author :: Text.Text
  , publishedTime :: Text.Text
  } deriving Show

data Article = Article
  { author :: Text.Text
  , bio :: Text.Text
  , link :: Text.Text
  , avatar :: Text.Text
  , slug :: Text.Text
  , date :: Text.Text
  , title :: Text.Text
  , sub :: Text.Text
  , body :: Text.Text
  , meta :: Meta
  } deriving Show

data Job = Job
  { url :: Text.Text
  , title :: Text.Text
  , location :: Text.Text
  , equity :: Text.Text
  , experience :: Text.Text
  , description :: Text.Text
  } deriving Show

data Location = Location
  { address :: Address
  , mailingList :: Address
  } deriving Show

data Candidate = Candidate
  { applicationTitle :: Text.Text
  , firstName :: Text.Text
  , lastName :: Text.Text
  , email :: Text.Text
  , phone :: Text.Text
  , reason :: Text.Text
  } deriving Show

data MailRenderer = MailRenderer
  { htmlRenderer :: Candidate -> Html ()
  , textRenderer :: Candidate -> Text.Lazy.Text
  }
