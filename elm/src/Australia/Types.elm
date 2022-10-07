module Australia.Types exposing (Model, Msg(..), View(..))

import Apply exposing (Applicant, Field(..), Job)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Element exposing (Element)
import File exposing (File)
import Http
import RemoteData exposing (WebData)
import Text exposing (Text)
import Url exposing (Url)


type alias Model =
    { jobs : WebData (Dict String Job)
    , gitVersion : String
    , applicant : Applicant
    , error : Maybe String
    , title : String
    , url : Url
    , key : Key
    , success : Maybe String
    , isPhoneMenuVisible : Bool
    }


type Msg
    = UploadResume
    | Resume File
    | ReceiveJobsData (WebData (Dict String Job))
    | SendApplicantData (Result Http.Error ())
    | Set Field String
    | Submit Job
    | PhoneMenuToggle


type View
    = ApplyView String
    | JobsView
