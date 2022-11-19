module Join.View exposing (view)

import Device
import Element
    exposing
        ( Attribute
        , Element
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , html
        , htmlAttribute
        , link
        , maximum
        , newTabLink
        , paddingXY
        , paragraph
        , px
        , row
        , spacing
        , spacingXY
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Font as Font
import Framework.Heading as Heading
import Html
import Html.Attributes as HtmlAttr
import Join.Types exposing (Model, Msg(..))
import Layout exposing (Layout, footer, menu, phoneMenu, topMenu)
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, css, hf, lineHeight, minW, wf)


view : Device.Device -> Model -> Layout Msg
view device model =
    let
        sectionBg =
            [ css "background" "#DAE9FF"
            , css "background" "linear-gradient(180deg, #FFFBF8 0%, #DAE9FF 102.99%)"
            ]

        render view_ =
            if model.isPhoneMenuVisible then
                column [ wf, hf, css "position" "relative" ] [ phoneMenu PhoneMenuToggle model.isPhoneMenuVisible ]
                    |> List.singleton

            else
                view_
    in
    { phone =
        render <|
            [ column
                [ wf
                , Font.family [ Font.typeface "Inter" ]
                ]
                [ row [ wf, hf ] [ toHeader device model ]
                , toView device
                ]
            , column
                ([ wf
                 , height fill
                 , Font.family [ Font.typeface "Inter" ]
                 ]
                    ++ sectionBg
                )
                [ jobsView device ]
            , column [ wf ] footer.phone
            ]
    , desktop =
        render <|
            [ column
                [ wf
                , Font.family [ Font.typeface "Inter" ]
                ]
                [ row [ wf, hf ] [ toHeader device model ]
                , toView device
                ]
            , column
                ([ wf
                 , height fill
                 , Font.family [ Font.typeface "Inter" ]
                 , centerX
                 ]
                    ++ sectionBg
                )
                [ jobsView device ]
            , column [ wf ] footer.desktop
            ]
    , tablet =
        render <|
            [ column
                [ wf
                , Font.family [ Font.typeface "Inter" ]
                ]
                [ row [ wf, hf ] [ toHeader device model ]
                , toView device
                ]
            , column
                ([ wf
                 , height fill
                 , Font.family [ Font.typeface "Inter" ]
                 , centerX
                 ]
                    ++ sectionBg
                )
                [ jobsView device ]
            , column [ wf ] footer.phone
            ]
    }


toView : Device.Device -> Element msg
toView device =
    column
        [ Background.color colors.cremeDark
        , wf
        , hf
        , Font.family [ Font.typeface "Inter" ]
        ]
        [ row [ wf ]
            [ row [ width <| fillPortion 2 ] [ Element.none ]
            , column [ width <| fillPortion 8 ] [ joinTeamBody device ]
            , row [ width <| fillPortion 2 ] [ Element.none ]
            ]
        ]


joinTeamBody : Device.Device -> Element msg
joinTeamBody device =
    let
        titleStyle =
            [ Font.center
            , Font.size 28
            , Font.semiBold
            , Font.color colors.primary
            ]

        interviewProcessSm =
            row [ centerX ]
                [ Element.image [ css "max-width" "100%", css "height" "auto" ] { src = "/static/images/interview-process-sm.png", description = "Flint interview process" }
                ]
    in
    column [ wf, centerX, paddingXY 0 48, spacingXY 0 56, Font.size 16 ]
        [ column [ centerX ]
            [ paragraph titleStyle
                [ text "We work with the very best" ]
            ]
        , wrappedRow [ alignTop, spacingXY 24 20 ]
            [ paragraph [ lineHeight 1.6, minW 300 ]
                [ text copy.paragraph1 ]
            , paragraph
                [ wf
                , hf
                , lineHeight 1.6
                , minW 300
                ]
                [ text copy.paragraph2
                , paragraph [] copy.other
                , paragraph
                    [ lineHeight 1.6
                    ]
                    [ text " We interview and make hires within a week from our first meet – it's a commitment." ]
                ]
            ]
        , case device of
            Device.Phone _ ->
                interviewProcessSm

            Device.Tablet _ ->
                interviewProcessSm

            _ ->
                row [ centerX ]
                    [ Element.image [ css "max-width" "100%", css "height" "auto" ] { src = "/static/images/interview-process.png", description = "Flint interview process" }
                    ]
        ]


toHeader : Device.Device -> Model -> Element Msg
toHeader device model =
    let
        bg =
            [ css "background" "#FFDCC9"
            , css "background" "linear-gradient(281.5deg, #FFDCC9 -0.43%, #C8BCC7 8.22%, #8284AF 27.81%, #6E74A9 52.4%, #6359A1 82.46%)"
            ]

        blobSrc =
            "/static/images/header-blob-blue.svg"

        title =
            copy.title
    in
    header device { title = title, menu = topMenu, bg = bg, blobSrc = blobSrc } model


header :
    Device.Device
    ->
        { title : String
        , menu : List ( String, Page )
        , bg : List (Attribute Msg)
        , blobSrc : String
        }
    -> Model
    -> Element Msg
header device { title, menu, bg, blobSrc } model =
    let
        blob =
            row [ css "position" "relative" ]
                [ row
                    [ alignTop
                    , htmlAttribute <| HtmlAttr.style "position" "relative"
                    , width (px 275)
                    , height (px 139)
                    ]
                    [ html <|
                        Html.img
                            [ HtmlAttr.src blobSrc
                            , HtmlAttr.style "width" "100%"
                            ]
                            []
                    ]
                , logo
                ]

        link : ( String, Page ) -> Element msg
        link ( label, page ) =
            Element.link
                []
                { url = toPath page
                , label =
                    el [ Font.center ] (text label)
                }

        rs =
            case device of
                Device.Phone _ ->
                    { titleFontSize = 36
                    }

                Device.Tablet _ ->
                    { titleFontSize = 32
                    }

                Device.Desktop _ ->
                    { titleFontSize = 44
                    }

                Device.NotSet ->
                    { titleFontSize = 0
                    }

        logo =
            row
                [ css "position" "absolute"
                , css "left" "44px"
                , css "top" "20px"
                , css "z-index" "100"
                ]
                [ Element.link [ wf ]
                    { url = toPath Home
                    , label =
                        Element.image
                            [ width (px 110), height (px 54) ]
                            { src = "/static/images/logo.svg?new", description = "Flint" }
                    }
                ]

        renderHamburgerMenu =
            case device of
                Device.Phone _ ->
                    phoneMenu PhoneMenuToggle model.isPhoneMenuVisible

                _ ->
                    Element.none
    in
    row ([ wf, css "position" "relative" ] ++ bg)
        [ renderHamburgerMenu
        , column [ css "position" "absolute", css "top" "0", css "left" "0" ]
            [ row [ css "width" "80%", css "height" "80%" ] [ blob ]
            ]
        , column
            [ alignTop, height (px 280), wf ]
            [ case device of
                Device.Phone _ ->
                    row [ wf, height <| fillPortion 4 ] [ Element.none ]

                _ ->
                    row [ wf, height <| fillPortion 4 ]
                        [ row [ wf ]
                            [ row [ width <| fillPortion 7 ] []
                            , row
                                [ width <| fillPortion 4
                                , spacing 32
                                , Font.color colors.white
                                , Font.letterSpacing 2
                                , Font.size 14
                                ]
                                [ row [ alignRight, spacingXY 36 0 ]
                                    (List.map (el (wf :: Styles.menu) << link) menu)
                                ]
                            , row [ width <| fillPortion 2 ] []
                            ]
                        ]
            , row [ wf, height <| fillPortion 8 ]
                [ el ([ wf, centerX, Font.size rs.titleFontSize ] ++ Heading.h1 ++ Styles.title)
                    (paragraph [ Font.center, Font.size rs.titleFontSize ] [ text title ])
                ]
            , case device of
                Device.Phone _ ->
                    Element.none

                _ ->
                    row [ wf, height <| fillPortion 2 ] []
            ]
        ]


jobsView : Device.Device -> Element Msg
jobsView device =
    let
        rsPadding =
            case device of
                Device.Phone _ ->
                    paddingXY 20 40

                _ ->
                    paddingXY 100 40

        btnConfig =
            { fontColor = colors.white
            , bgColor = colors.carminePink
            }

        linkToGreenhouse =
            newTabLink
                (centerY :: centerX :: Font.size 15 :: Styles.btnFilled btnConfig)
                { url = "https://boards.greenhouse.io/flint"
                , label = paragraph [ Font.center ] [ text <| "Open Positions" ]
                }
    in
    column
        [ hf
        , centerX
        , width <| maximum 1500 fill
        ]
        [ column
            [ spacingXY 0 20
            , rsPadding
            , wf
            , centerX
            ]
          <|
            [ column [ spacing 40, paddingXY 0 40, width (fill |> Element.maximum 1000), centerX ]
                [ linkToGreenhouse ]
            ]
        ]


copy : { desktopHeader : String, phoneHeader : String, paragraph1 : String, paragraph2 : String, why : String, title : String, pageTitle : String, other : List (Element msg) }
copy =
    { desktopHeader = "We work with the very best."
    , phoneHeader = "We work with the very best."
    , paragraph1 = "At Flint, we're committed to hiring the best people to build our teams. Building great products takes smart, disciplined, and empathetic individuals who can understand what job the products need to get done and imagine innovative ways to achieve it. Thus we designed the hiring process to help us identify those people."
    , paragraph2 = "We foster a culture of respect, dialogue and growth where our team members can engage in a continuous conversation about product, engineering, and learning."
    , why = "Why do you want to work at Flint?"
    , title = "Join the Team"
    , pageTitle = "Join the Team - Flint"
    , other =
        [ text " "
        , link Styles.link
            { url = toPath (Blog "culture")
            , label = text "Read more about our values and culture."
            }
        , text " "
        ]
    }
