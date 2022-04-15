module Faq.Update exposing (..)

import Faq.Types exposing (Model, Msg(..))
import Return exposing (Return, singleton)

init : Model
init =
    { topic = ""
    , title = "FAQ for Internationally Educated Nurses"
    , selectedTopic = 0
    }


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        Select topicId ->
            singleton { model | selectedTopic = topicId }

        Hide ->
          singleton { model | selectedTopic = -1 }