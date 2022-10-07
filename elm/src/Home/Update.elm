module Home.Update exposing (init, update)

import Home.Types exposing (Model, Msg)
import Return exposing (Return, singleton)


init : Model
init =
    { topic = ""
    , title = "Flint - Securing Nurses for Your Future"
    , isPhoneMenuVisible = False
    }


update : Msg -> Model -> Return Msg Model
update _ model =
    singleton { model | isPhoneMenuVisible = not model.isPhoneMenuVisible }
