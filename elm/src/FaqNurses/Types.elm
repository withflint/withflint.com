module FaqNurses.Types exposing (Faq, FormattedText(..), Model, Msg(..))


type alias Model =
    { title : String
    , heroTitle : String
    , faqs : List Faq
    }


type Msg
    = ToggleVisibilty Int -- faq.id


type alias Faq =
    { id : Int
    , question : String
    , answer : List FormattedText
    , isVisible : Bool
    }


type FormattedText
    = Paragraph String
    | ListItem String
