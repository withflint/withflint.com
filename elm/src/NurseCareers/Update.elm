module NurseCareers.Update exposing (init, update)

import Browser.Navigation exposing (load)
import NurseCareers.Types exposing (Model, Msg(..))
import Ports
import Return exposing (Return, return, singleton)
import Url


init : Return Msg Model
init =
    singleton
        { email = Nothing
        , error = Nothing
        }


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        ApplyButtonClicked ->
            singleton model
                |> Return.command (load "https://docs.google.com/forms/d/e/1FAIpQLSc4EJ7dU87Yuss7zBxtm3vAMyAdVlA2sTSvd05A3dBVlsgQAA/viewform?usp=sf_link")

        GotURL res ->
            case res of
                Err _ ->
                    singleton { model | error = Just "Sorry, we can't process your application becasue of an network issue, please try again later" }

                Ok urlString ->
                    case Url.fromString urlString of
                        Just _ ->
                            return { model | error = Nothing } (load urlString)

                        Nothing ->
                            singleton { model | error = Just "sorry we can't process your application right now, please try again later" }

        ToggleNavMenu ->
            return model (Ports.toggleNavMenu ())
