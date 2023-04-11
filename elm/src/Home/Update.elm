module Home.Update exposing (init, update)

import Home.Types exposing (Model, Msg)
import Ports
import Return exposing (Return, return)


init : Model
init =
    { topic = ""
    , title = "Flint - Securing Nurses for Your Future"
    }


update : Msg -> Model -> Return Msg Model
update _ model =
    return model (Ports.toggleNavMenu ())
