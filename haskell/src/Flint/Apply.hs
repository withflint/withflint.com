module Flint.Apply where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encoding
import Lucid
import Web.Scotty
import Network.Wai
import Network.Mail.Mime
import Network.Mail.SMTP (sendMailSTARTTLS)
import Flint.Types
import Flint.Utils
import Text.Shakespeare.Text
import Control.Monad.IO.Class
import Network.Wai.Parse

data Location = Location
  { address :: Address
  , mailingList :: Address
  } deriving Show

data Candidate = Candidate
  { applicationTitle :: Text
  , firstName :: Text
  , lastName :: Text
  , email :: Text
  , phone :: Text
  , reason :: Text
  } deriving Show

getCandidate :: ActionM Candidate
getCandidate = do
  applicationTitle <- param "applicationTitle"
  firstName <- param "firstName"
  lastName <- param "lastName"
  email <- param "email"
  phone <- param "phone"
  reason <- param "reason"
  pure Candidate { .. }

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

careersBody :: Candidate -> Html ()
careersBody (Candidate { .. }) = do
  toHtml [st|Hello #{firstName},|]
  
  br_ []
  br_ []

  toHtml [st|Thank you for your interest in the #{applicationTitle} position at Flint.|]

  br_ []

  "I will review your candidacy and get back to you shortly."

  br_ []
  br_ []

  "Kind Regards,"

  br_ []
  br_ []

  "Simon Green"

  br_ []

  "Head of Product at "
  a_ [ href_ "https://withflint.com/" ] "Flint"

careersEmail :: Location
careersEmail = Location
  { address = Address (Just "Simon Green") "simon@withflint.com"
  , mailingList = Address Nothing "careers+ws@withflint.com"
  }

healthCareBody :: Candidate -> Html ()
healthCareBody (Candidate { .. }) = do
  toHtml [st|Hello #{firstName},|]
  
  br_ []
  br_ []

  toHtml [st|Thank you for your interest in the #{applicationTitle} position at Flint.|]

  br_ []

  "We will review your candidacy and get back to you shortly."

  br_ []
  br_ []

  "Kind Regards,"

  br_ []
  br_ []

  "the Talent Team at "
  a_ [ href_ "https://withflint.com/" ] "Flint"

healthCareEmail :: Location
healthCareEmail = Location
  { address = Address (Just "Flint Talent Team") "talent@withflint.com"
  , mailingList = Address Nothing "apply@withflint.com"
  }
  
apply :: Location -> Candidate -> (Candidate -> Html ()) -> Action
apply location candidate@(Candidate { .. }) genBody = do
  attachments <- map fileToAttachment <$> files

  let candidateAddress = Address (Just [st|#{firstName} #{lastName}|]) email
  
  let subject = [st|Flint - New Application : #{firstName} #{lastName}, #{applicationTitle}|]
  let htmlBodyForNotification = renderText $ mailBody candidate
  let notification = simpleMailInMemory location.address location.mailingList subject "" htmlBodyForNotification attachments
  let replyTo = ("Reply-To", renderAddress candidateAddress)

  let htmlBodyForCandidate = renderText $ genBody candidate
  let emailToCandidate = simpleMailInMemory location.address candidateAddress "Thank you for applying" "" htmlBodyForCandidate []
  
  liftIO do
    sendMailSTARTTLS "smtp-relay.gmail.com" notification { mailHeaders = [ replyTo ] }
    sendMailSTARTTLS "smtp-relay.gmail.com" emailToCandidate
