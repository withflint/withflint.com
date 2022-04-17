module FaqNurses.Update exposing (..)

import FaqNurses.Types exposing (Model, Msg(..))
import Return exposing (Return, singleton)


init : Return Msg Model
init =
    singleton
        { title = "FAQ for Internationally Educated Nurses - Flint"
        , heroTitle = "FAQ for Internationally Educated Nurses"
        , faqs = []
        }


update : Msg -> Model -> Return Msg Model
update msg model =
    singleton model
