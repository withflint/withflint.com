module FaqNurses.Types exposing (Faq, Model, Msg(..))


type alias Model =
    { title : String
    , heroTitle : String
    , faqs : List Faq
    }


type Msg
    = NoOp


type alias Faq =
    { question : String
    , answer : String
    , isVisible : Bool
    }
