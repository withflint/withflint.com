module About.Update exposing (init, update)

import About.Types exposing (Model, Msg(..))
import Ports
import Return exposing (Return, return, singleton)


init : Model
init =
    { topic = ""
    , title = "About Us - Flint"
    , isPhoneMenuVisible = False
    , isProfileVisible = False
    }


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        PhoneMenuToggle ->
            singleton { model | isPhoneMenuVisible = not model.isPhoneMenuVisible }

        ShowProfile ->
            singleton { model | isProfileVisible = not model.isProfileVisible }

        ToggleNavMenu ->
            return model (Ports.toggleNavMenu ())
