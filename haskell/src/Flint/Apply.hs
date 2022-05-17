module Flint.Apply where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encoding
import Lucid
import Web.Scotty
import Network.Wai
import Network.Mail.Mime
import Network.Mail.SMTP
import Flint.Types
import Flint.Utils
import Text.Shakespeare.Text
import Control.Monad.IO.Class
import Network.Wai.Parse

data Location = Location
  { who :: Text
  , from :: Text
  , to :: Text
  } deriving (Show, Eq)

data Candidate = Candidate
  { applicationTitle :: Text
  , firstName :: Text
  , lastName :: Text
  , email :: Text
  , phone :: Text
  , reason :: Text
  } deriving (Show, Eq)

mailBody :: Candidate -> Html ()
mailBody (Candidate { .. }) = do
  toHtml reason
  
  br_ []
  br_ []
    
  toHtml [st|#{firstName} #{lastName}|]

  br_ []

  toHtml phone

  br_ []

  toHtml email
  
apply :: Location -> Candidate -> ActionM Mail
apply location candidate@(Candidate { .. }) = do
  attachments <- map fileToAttachment <$> files
  
  let addr = Address (Just [st|#{firstName} #{lastName}|]) location.who
  let subject = [st|Flint - New Application : #{firstName} #{lastName}, #{applicationTitle}|]
  let htmlBody = renderText $ mailBody candidate
  let jobs = simpleMailInMemory addr addr subject htmlBody htmlBody attachments
  let replyTo = ("Reply-To", [st|#{firstName} #{lastName} <#{email}>|])
  -- liftIO $ sendMailTLS' "smtp-relay.gmail.com" 587 jobs
  --   { mailHeaders = [replyTo]
  --   }
  -- pure ()
  pure jobs
    { mailHeaders = [replyTo]
    }
