module Home.Types exposing (Model, Msg(..))


type alias Model =
    { topic : String
    , title : String
    }


type Msg
    = ToggleNavMenu
