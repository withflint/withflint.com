module Faq.Types exposing (..)

type Msg
    = Select Int
    | Hide

type alias Model =
    { topic : String
    , title : String
    , selectedTopic : Int
    }
