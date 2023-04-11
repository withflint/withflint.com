module Partnerships.Update exposing (init, update)

import Partnerships.Types exposing (Model, Msg)
import Ports
import Return exposing (Return, return)


init : Model
init =
    { topic = ""
    , title = "Partnerships - Flint"
    , isPhoneMenuVisible = False
    }


update : Msg -> Model -> Return Msg Model
update _ model =
    return model (Ports.toggleNavMenu ())
