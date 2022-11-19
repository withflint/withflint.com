module Partners.Types exposing (Config, Model, Msg(..), View(..))

import Apply exposing (Applicant, Field, Job)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import File exposing (File)
import Http
import RemoteData exposing (WebData)
import Url exposing (Url)


type alias Config =
    { page : String
    , endpoint : String
    , apply : String
    }


type alias Model =
    { jobs : WebData (Dict String Job)
    , gitVersion : String
    , applicant : Applicant
    , error : Maybe String
    , title : String
    , url : Url
    , key : Key
    , view : View
    , config : Config
    , success : Maybe String
    , isPhoneMenuVisible : Bool
    }


type Msg
    = UploadResume
    | Resume File
    | ReceiveJobsData (WebData (Dict String Job))
    | SendApplicantData (Result Http.Error ())
    | Set Field String
    | SwitchView View
    | Apply Bool String
    | Submit Job
    | PhoneMenuToggle


type View
    = ApplyView String
    | JobsView