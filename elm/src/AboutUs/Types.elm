module AboutUs.Types exposing (Model, Msg(..))


type alias Model =
    { topic : String
    , title : String
    , isPhoneMenuVisible : Bool
    , isProfileVisible : Bool
    }


type Msg
    = PhoneMenuToggle
    | ShowProfile
