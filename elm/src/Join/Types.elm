module Join.Types exposing (Config, Model, Msg(..), View(..))

import Apply exposing (Applicant, Job)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
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
    = SwitchView View
    | PhoneMenuToggle


type View
    = ApplyView
    | JobsView
