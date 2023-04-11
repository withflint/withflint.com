module FaqNurses.Types exposing (Faq, FormattedText(..), Model, Msg(..))


type alias Model =
    { title : String
    , heroTitle : String
    , faqs : List Faq
    , isPhoneMenuVisible : Bool
    }


type Msg
    = NoOp
    | PhoneMenuToggle
    | ToggleNavMenu


type alias Faq =
    { id : Int
    , question : String
    , answer : List FormattedText
    }


type FormattedText
    = Paragraph String
    | ListItem String
    | OrderedItem ( Int, String )
