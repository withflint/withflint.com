module Flint.Apply where

import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encoding
import Flint.Types
import Lucid
import Network.Mail.Mime
import Network.Mail.SMTP (sendMailSTARTTLS)
import Network.Wai
import Network.Wai.Parse
import Network.Wai.Parse (FileInfo (..))
import Text.Shakespeare.Text
import Web.Scotty

getCandidate :: ActionM Candidate
getCandidate = do
  applicationTitle <- param "applicationTitle"
  firstName <- param "firstName"
  lastName <- param "lastName"
  email <- param "email"
  phone <- param "phone"
  reason <- param "reason"
  pure Candidate {..}

mailHtmlBody :: Candidate -> Html ()
mailHtmlBody (Candidate {..}) = do
  toHtml reason

  br_ []
  br_ []

  toHtml [st|#{firstName} #{lastName}|]

  br_ []

  toHtml phone

  br_ []

  toHtml email

mailTextBody :: Candidate -> Text.Lazy.Text
mailTextBody (Candidate {..}) =
  [lbt|#{reason}
      |
      |#{firstName} #{lastName}
      |
      |#{phone}
      |
      |#{email}
      |]

mailRenderer :: MailRenderer
mailRenderer =
  MailRenderer
    { htmlRenderer = mailHtmlBody
    , textRenderer = mailTextBody
    }

careersHtmlBody :: Candidate -> Html ()
careersHtmlBody (Candidate {..}) = do
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
  a_ [href_ "https://withflint.com/"] "Flint"

careersTextBody :: Candidate -> Text.Lazy.Text
careersTextBody (Candidate {..}) =
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
careersRenderer =
  MailRenderer
    { htmlRenderer = careersHtmlBody
    , textRenderer = careersTextBody
    }

careersEmail :: Location
careersEmail =
  Location
    { address = Address (Just "Simon Green") "simon@withflint.com"
    , mailingList = Address Nothing "join+ws@withflint.com"
    }

nurseSuccessHtmlBody :: Candidate -> Html ()
nurseSuccessHtmlBody (Candidate {..}) = do
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

  "Nurse Success at "
  a_ [href_ "https://withflint.com/"] "Flint"

nurseSuccessHtmlBodySpanish :: Candidate -> Html ()
nurseSuccessHtmlBodySpanish (Candidate {..}) = do
  toHtml [st|Hola #{firstName},|]

  br_ []
  br_ []

  toHtml [st|Gracias por su interés en el puesto #{applicationTitle} en Flint.|]

  br_ []

  "Revisaremos su candidatura y nos pondremos en contacto con usted en breve."

  br_ []
  br_ []

  "Saludos cordiales,"

  br_ []
  br_ []

  "Nurse Success en "
  a_ [href_ "https://withflint.com/"] "Flint"

nurseSuccessTextBody :: Candidate -> Text.Lazy.Text
nurseSuccessTextBody (Candidate {..}) =
  [lbt|Hello #{firstName},
      |
      |Thank you for your interest in the #{applicationTitle} position at Flint.
      |We will review your candidacy and back to you shortly.
      |
      |Kind Regards,
      |
      |Nurse Success at Flint
      |https://withflint.com
      |]

nurseSuccessTextBodySpanish :: Candidate -> Text.Lazy.Text
nurseSuccessTextBodySpanish (Candidate {..}) =
  [lbt|Hola #{firstName},
      |
      |Gracias por su interés en el puesto #{applicationTitle} en Flint.
      |Revisaremos su candidatura y le responderemos en breve.
      |
      |Saludos cordiales,
      |
      |Nurse Success en Flint
      |https://withflint.com
      |]

nurseSuccessEmail :: MailRenderer
nurseSuccessEmail =
  MailRenderer
    { htmlRenderer = nurseSuccessHtmlBody
    , textRenderer = nurseSuccessTextBody
    }

nurseSuccessEmailSpanish :: MailRenderer
nurseSuccessEmailSpanish =
  MailRenderer
    { htmlRenderer = nurseSuccessHtmlBodySpanish
    , textRenderer = nurseSuccessTextBodySpanish
    }

nurseSuccess :: Location
nurseSuccess =
  Location
    { address = Address (Just "Flint Nurse Success") "success@withflint.com"
    , mailingList = Address Nothing "apply@withflint.com"
    }

apply :: Location -> Candidate -> MailRenderer -> ActionM ()
apply location candidate@(Candidate {..}) renderer = do
  attachments <- map fileToAttachment <$> files

  let candidateAddress = Address (Just [st|#{firstName} #{lastName}|]) email

  let subject = [st|Flint - New Application : #{firstName} #{lastName}, #{applicationTitle}|]

  let htmlBodyForNotification = renderText $ mailRenderer.htmlRenderer candidate
  let textBodyForNotification = mailRenderer.textRenderer candidate

  let notification =
        simpleMailInMemory
          location.mailingList
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
          candidateAddress
          location.address
          "Thank you for applying"
          textBodyForCandidate
          htmlBodyForCandidate
          []

  liftIO do
    sendMailSTARTTLS
      "smtp-relay.gmail.com"
      notification
        { mailHeaders = replyTo : notification.mailHeaders
        }

    sendMailSTARTTLS "smtp-relay.gmail.com" emailToCandidate

type Attachment = (Text, Text, ByteString)

fileToAttachment :: Web.Scotty.File -> Attachment
fileToAttachment (fieldName, info) =
  ( Data.Text.Encoding.decodeUtf8 $ fileContentType info
  , Data.Text.Encoding.decodeUtf8 $ fileName info
  , fileContent info
  )
