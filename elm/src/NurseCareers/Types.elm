module NurseCareers.Types exposing (Model, Msg(..))

import Http


type alias Model =
    { email : Maybe String
    , error : Maybe String
    }


type Msg
    = EmailInputChanged String
    | ApplyButtonClicked
    | GotURL (Result Http.Error String)
    | ToggleNavMenu
