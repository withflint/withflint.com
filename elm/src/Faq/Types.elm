module Faq.Types exposing (..)


type Msg
    = Select Int
    | Hide


type alias Model =
    { title : String
    , selectedTopic : Int
    , faqs : List ( String, String )
    }
