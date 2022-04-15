module View exposing (view)

import Blog.View
import Contact.View
import Element
    exposing
        ( Element
        , alignLeft
        , centerY
        , column
        , el
        , fill
        , height
        , link
        , padding
        , paragraph
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Home.View
import Faq.View
import Html exposing (Html)
import Jobs.View
import Layout exposing (layout)
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors)
import Types exposing (Model, Msg(..))


view : Model -> { title : String, body : List (Html Types.Msg) }
view model =
    { title = model.title
    , body =
        [ Element.layout [ Styles.font, width fill, height fill ] <|
            el [ width fill, height fill ] (renderRoute model)
        ]
    }


renderRoute : Types.Model -> Element Types.Msg
renderRoute model =
    case model.router.page of
        Home ->
            layout model.device <| Home.View.view model.home

        Contact ->
            layout model.device <| Contact.View.view model.contact

        Faq ->
            Element.map MsgForFaq <| layout model.device <| Faq.View.view model.faq

        Jobs _ ->
            Element.map MsgForJobs <| layout model.device <| Jobs.View.view model.jobs

        HealthCare _ ->
            Element.map MsgForHealthCare <| layout model.device <| Jobs.View.view model.healthCare

        Blog _ ->
            Element.map MsgForBlog <| layout model.device <| Blog.View.view model.blog

        NotFound ->
            notFound


notFound : Element msg
notFound =
    column
        [ Background.color colors.blue1
        , width fill
        , height fill
        , padding 20
        , Font.color colors.white3
        , spacing 20
        ]
        [ row []
            [ link []
                { url = toPath Home
                , label =
                    Element.image
                        [ centerY
                        , alignLeft
                        , width (px 100)
                        , height (px 50)
                        ]
                        { src = "/static/images/logo-white.svg"
                        , description = "Flint"
                        }
                }
            ]
        , row
            [ Font.size 36
            ]
            [ paragraph []
                [ text "404: Oops! We could not find that page. "
                ]
            ]
        , row
            [ Font.size 20
            ]
            [ paragraph []
                [ link
                    (Styles.link
                        ++ [ Font.color colors.white3 ]
                    )
                    { url = toPath Home
                    , label = text "Go home!"
                    }
                ]
            ]
        ]
