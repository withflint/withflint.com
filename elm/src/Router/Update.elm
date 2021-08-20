module Router.Update exposing (init, update)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation exposing (Key, load, pushUrl)
import Router.Routes exposing (Page(..), routes)
import Router.Types exposing (Eff(..), Model, Msg(..))
import SubCmd
import SubReturn exposing (SubReturn, returnCmd, returnSub, singletonSub)
import Task
import Url exposing (Url)
import Url.Parser exposing (parse)


init : Url -> Key -> SubReturn Msg Eff Model
init url key =
    singletonSub
        { page = Maybe.withDefault NotFound <| parse routes url
        , key = key
        }


update : Msg -> Model -> SubReturn Msg Eff Model
update msg model =
    case msg of
        OnUrlChange url ->
            returnSub { model | page = Maybe.withDefault NotFound <| parse routes url } <|
                SubCmd.batch
                    [ SubCmd.cmd <| Task.perform (always NoOp) (Browser.Dom.setViewport 0 0)
                    , SubCmd.effect <| UrlChange url
                    ]

        OnUrlRequest urlRequest ->
            case urlRequest of
                Internal url ->
                    returnCmd model <| pushUrl model.key <| Url.toString url

                External url ->
                    returnCmd model <| load url

        NoOp ->
            singletonSub model
