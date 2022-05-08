module Jobs.Types exposing (Applicant, Config, Copy, Field(..), Job, Model, Msg(..), View(..))

import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Element exposing (Element)
import File exposing (File)
import Http
import Text exposing (Text)
import Url exposing (Url)


type alias Applicant =
    { firstName : Text
    , lastName : Text
    , email : Text
    , phone : Text
    , resume : Maybe File
    , reason : Text
    }


type alias Config =
    { page : String
    , endpoint : String
    , copy : Copy
    , apply : String
    }


type alias Copy =
    { desktopHeader : String
    , phoneHeader : String
    , paragraph1 : String
    , paragraph2 : String
    , why : String
    , title : String
    , pageTitle : String
    , other : Maybe (List (Element Msg))
    }


type Field
    = FirstName
    | LastName
    | Email
    | Phone
    | Reason


type alias Job =
    { url : String
    , title : String
    , location : String
    , equity : String
    , experience : String
    , description : String
    }


type alias Model =
    { jobs : Dict String Job
    , gitVersion : String
    , applicant : Applicant
    , error : Maybe String
    , title : String
    , url : Url
    , key : Key
    , view : View
    , config : Config
    , success : Maybe String
    }


type Msg
    = UploadResume
    | Resume File
    | ReceiveJobsData (Result Http.Error String)
    | SendApplicantData (Result Http.Error ())
    | Set Field String
    | SwitchView View
    | Apply Bool String
    | Submit Job


type View
    = ApplyView String
    | JobsView
