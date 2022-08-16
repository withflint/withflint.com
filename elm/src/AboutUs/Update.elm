module AboutUs.Update exposing (init, update)

import AboutUs.Types exposing (Model, Msg(..))
import Return exposing (Return, singleton)


init : Model
init =
    { topic = ""
    , title = "About Us - Flint"
    , isPhoneMenuVisible = False
    }


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        PhoneMenuToggle ->
            singleton { model | isPhoneMenuVisible = not model.isPhoneMenuVisible }
