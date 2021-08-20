module Router.Types exposing (Eff(..), Model, Msg(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Router.Routes exposing (Page)
import Url exposing (Url)


type alias Model =
    { page : Page
    , key : Key
    }


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | NoOp


type Eff
    = UrlChange Url
