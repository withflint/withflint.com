module Home.Update exposing (init, update)

import Home.Types exposing (Model, Msg(..))
import Layout exposing (phoneMenu)
import Return exposing (Return, return, singleton)


init : Model
init =
    { topic = ""
    , title = "Flint - Securing Nurses for Your Future"
    , isPhoneMenuVisible = False
    }


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        PhoneMenuToggle ->
            singleton { model | isPhoneMenuVisible = not model.isPhoneMenuVisible }



-- |> Debug.log "phoneMenu"
