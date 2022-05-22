module Flint.Apply where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
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

getCandidate :: ActionM Candidate
getCandidate = do
  applicationTitle <- param "applicationTitle"
  firstName <- param "firstName"
  lastName <- param "lastName"
  email <- param "email"
  phone <- param "phone"
  reason <- param "reason"
  pure Candidate { .. }

mailHtmlBody :: Candidate -> Html ()
mailHtmlBody (Candidate { .. }) = do
  toHtml reason
  
  br_ []
  br_ []
    
  toHtml [st|#{firstName} #{lastName}|]

  br_ []

  toHtml phone

  br_ []

  toHtml email

mailTextBody :: Candidate -> Text.Lazy.Text
mailTextBody (Candidate { .. }) =
  [lbt|#{reason}
      |
      |#{firstName} #{lastName}
      |
      |#{phone}
      |
      |#{email}
      |]

mailRenderer :: MailRenderer
mailRenderer = MailRenderer
  { htmlRenderer = mailHtmlBody
  , textRenderer = mailTextBody
  }

careersHtmlBody :: Candidate -> Html ()
careersHtmlBody (Candidate { .. }) = do
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

careersTextBody :: Candidate -> Text.Lazy.Text
careersTextBody (Candidate { .. }) =
  [lbt|Hello #{firstName},
      |
      |Thank you for your interest in the #{applicationTitle} position at Flint.
      |I will review your candidacy and get back to you shortly.
      |
      |Kind Regards,
      |Simon Green
      |
      |Head of Product at Flint
      |https://withflint.com
      |]

careersRenderer :: MailRenderer
careersRenderer = MailRenderer
  { htmlRenderer = careersHtmlBody
  , textRenderer = careersTextBody
  }

careersEmail :: Location
careersEmail = Location
  { address = Address (Just "Simon Green") "simon@withflint.com"
  , mailingList = Address Nothing "join+ws@withflint.com"
  }

healthCareHtmlBody :: Candidate -> Html ()
healthCareHtmlBody (Candidate { .. }) = do
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

healthCareTextBody :: Candidate -> Text.Lazy.Text
healthCareTextBody (Candidate { .. }) =
  [lbt|Hello #{firstName},
      |
      |Thank you for your interest in the #{applicationTitle} position at Flint.
      |We will review your candidacy and back to you shortly.
      |
      |Kind Regards,
      |
      |the Talent Team at Flint
      |https://withflint.com
      |]

healthCareRenderer :: MailRenderer
healthCareRenderer = MailRenderer
  { htmlRenderer = healthCareHtmlBody
  , textRenderer = healthCareTextBody
  }

healthCareEmail :: Location
healthCareEmail = Location
  { address = Address (Just "Flint Talent Team") "talent@withflint.com"
  , mailingList = Address Nothing "apply@withflint.com"
  }


apply :: Location -> Candidate -> MailRenderer -> Action
apply location candidate@(Candidate { .. }) renderer = do
  attachments <- map fileToAttachment <$> files

  let candidateAddress = Address (Just [st|#{firstName} #{lastName}|]) email
  
  let subject = [st|Flint - New Application : #{firstName} #{lastName}, #{applicationTitle}|]

  let htmlBodyForNotification = renderText $ mailRenderer.htmlRenderer candidate
  let textBodyForNotification = mailRenderer.textRenderer candidate
  
  let notification =
        simpleMailInMemory
          location.address
          location.mailingList
          subject
          textBodyForNotification
          htmlBodyForNotification
          attachments
          
  let replyTo = ("Reply-To", renderAddress candidateAddress)

  let htmlBodyForCandidate = renderText $ renderer.htmlRenderer candidate
  let textBodyForCandidate = renderer.textRenderer candidate
  
  let emailToCandidate =
        simpleMailInMemory
          location.address
          candidateAddress
          "Thank you for applying"
          textBodyForCandidate
          htmlBodyForCandidate
          []
  
  liftIO do
    sendMailSTARTTLS "smtp-relay.gmail.com" notification {
      mailHeaders = [ replyTo ]
    }
    
    sendMailSTARTTLS "smtp-relay.gmail.com" emailToCandidate
