module View exposing (view)

import About.View
import Australia.View
import Blog.View
import Canada.View
import Chile.View
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
import Element.Font as Font
import FaqNurses.View
import Home.View
import Html exposing (Html)
import Jobs.View
import Layout exposing (layout)
import Mexico.View
import Partnerships.View
import Router.Routes exposing (Page(..), toPath)
import Singapore.View
import Styles exposing (colors, headerGradientBackground, pt)
import Types exposing (Model, Msg(..))


view : Model -> { title : String, body : List (Html Types.Msg) }
view model =
    { title = model.title
    , body =
        [ Element.layout (Styles.paragraph ++ [ Styles.font, width fill, height fill ]) <|
            el [ width fill, height fill ] (renderRoute model)
        ]
    }


renderRoute : Types.Model -> Element Types.Msg
renderRoute model =
    case model.router.page of
        Home ->
            Element.map MsgForHome <| layout model.device <| Home.View.view model.home model.device

        About ->
            Element.map MsgForAboutUs <| layout model.device <| About.View.view model.device model.aboutUs

        Partnerships ->
            Element.map MsgForPartnerships <| layout model.device <| Partnerships.View.view model.device model.partnerships

        JoinTheTeam _ ->
            Element.map MsgForJobs <| layout model.device <| Jobs.View.view model.device model.jobs

        NurseCareers _ ->
            Element.map MsgForHealthCare <| layout model.device <| Jobs.View.view model.device model.healthcare

        Australia ->
            Element.map MsgForAustralia <| layout model.device <| Australia.View.view model.device model.australia

        Mexico ->
            Element.map MsgForMexico <| layout model.device <| Mexico.View.view model.device model.mexico

        Canada ->
            Element.map MsgForCanada <| layout model.device <| Canada.View.view model.device model.canada

        Chile ->
            Element.map MsgForChile <| layout model.device <| Chile.View.view model.device model.chile

        Singapore ->
            Element.map MsgForSingapore <| layout model.device <| Singapore.View.view model.device model.singapore

        Blog _ ->
            Element.map MsgForBlog <| layout model.device <| Blog.View.view model.device model.blog

        FaqNurses ->
            Element.map MsgForFaqNurses <| layout model.device <| FaqNurses.View.view model.device model.faqNurses

        NotFound ->
            notFound


notFound : Element msg
notFound =
    column
        ([ width fill
         , height fill
         , padding 20
         , Font.color colors.cremeLight
         , spacing 20
         ]
            ++ headerGradientBackground
        )
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
                        { src = "/static/images/logo-white.svg?new"
                        , description = "Flint"
                        }
                }
            ]
        , row
            [ Font.size 36
            , Font.color colors.cremeLight
            , pt 32
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
                        ++ [ Font.color colors.cremeLight
                           ]
                    )
                    { url = toPath Home
                    , label = text "Go home!"
                    }
                ]
            ]
        ]
