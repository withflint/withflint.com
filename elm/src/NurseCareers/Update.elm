module NurseCareers.Update exposing (init, update)

import Base64
import Browser.Navigation exposing (load)
import Email
import Http
import Json.Decode as Decode
import NurseCareers.Types exposing (Model, Msg(..))
import Ports
import Return exposing (Return, singleton, return)
import Url
import Url.Builder exposing (crossOrigin)


init : Return Msg Model
init =
    singleton
        { email = Nothing
        , error = Nothing
        }


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        EmailInputChanged email ->
            case email of
                "" ->
                    singleton { model | email = Nothing }

                other ->
                    singleton { model | email = Just other }

        ApplyButtonClicked ->
            case model.email |> Maybe.andThen Email.fromString of
                Just email ->
                    let
                        url =
                            crossOrigin "https://app.withflint.com" [ "valve", "apply", "email", Base64.encode << Email.toString <| email ] []
                    in
                    singleton { model | error = Nothing }
                        |> command Ports.candidateApply
                        |> command
                            (Http.post
                                { url = url
                                , body = Http.emptyBody
                                , expect = Http.expectJson GotURL Decode.string
                                }
                            )

                Nothing ->
                    singleton { model | error = Just "please input a valid email" }

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
