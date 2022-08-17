module AboutUs.View exposing (view)

import AboutUs.Types exposing (Model, Msg(..))
import Device
import Element
    exposing
        ( Element
        , alignRight
        , alignTop
        , centerX
        , column
        , el
        , fill
        , fillPortion
        , height
        , html
        , htmlAttribute
        , paddingXY
        , paragraph
        , px
        , row
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes as HtmlAttr
import Layout exposing (Layout, phoneMenu)
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, css, hf, lineHeight, palette, pt, wf)


view : Device.Device -> Model -> Layout Msg
view device model =
    let
        render view_ =
            -- Render with phoneMenu
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
                , height fill
                ]
                [ header device model
                , body device model
                ]
            ]
    , tablet =
        render <|
            [ column
                [ wf
                , height fill
                ]
                [ header device model
                , body device model
                ]
            ]
    , desktop =
        render <|
            [ column
                [ wf
                , height fill
                ]
                [ header device model
                , body device model
                ]
            ]
    }


body : Device.Device -> Model -> Element msg
body device model =
    let
        sectionBg =
            [ css "background" "#FCE5D9"
            , css "background" "linear-gradient(180deg, #FFFBF8 0%, #DAE9FF 102.99%)"
            ]
    in
    column
        [ Background.color colors.cremeDark
        , wf
        , hf
        , Font.family [ Font.typeface "Inter" ]
        ]
        [ row
            ([ wf ]
             --:: sectionBg
            )
            [ row [ width <| fillPortion 2 ] [ Element.none ]
            , column [ width <| fillPortion 8 ]
                [ aboutFlint device model
                , aboutTeam device model
                ]
            , row [ width <| fillPortion 2 ] [ Element.none ]
            ]
        ]


aboutTeam : Device.Device -> Model -> Element msg
aboutTeam device model =
    let
        titleStyle =
            [ Font.center
            , Font.size 28
            , Font.semiBold
            , Font.color palette.primary
            ]

        rsJustify =
            case device of
                Device.Phone _ ->
                    Font.center

                _ ->
                    Font.justify
    in
    column [ wf, centerX, paddingXY 0 48, spacingXY 0 48 ]
        [ column [ centerX ]
            [ paragraph titleStyle
                [ text "Collaborative and multicultural team" ]
            ]
        , column [ spacingXY 0 12 ]
            [ paragraph [ Font.center, pt 12, rsJustify, lineHeight 1.6 ]
                [ text "As agents of change, we are deeply aligned with the healthcare industry's motivation.  We enable people to give their best by removing pain points and obstacles. Our team has experienced the hardships of the medical industry and immigration but also knows the silver lining of great work, caring for others, and finding community." ]
            ]
        ]


aboutFlint : Device.Device -> Model -> Element msg
aboutFlint device model =
    let
        titleStyle =
            [ Font.center
            , Font.size 28
            , Font.semiBold
            , Font.color palette.primary
            ]

        rsJustify =
            case device of
                Device.Phone _ ->
                    Font.center

                _ ->
                    Font.justify
    in
    column [ wf, centerX, paddingXY 0 48, spacingXY 0 48 ]
        [ column [ centerX ]
            [ paragraph titleStyle
                [ text "Working on the future of nursing" ]
            ]
        , column [ spacingXY 0 12 ]
            [ paragraph [ Font.center, pt 12, rsJustify, lineHeight 1.6 ]
                [ text "Flint was created to make the world a better place by connecting and improving lives. From those struggling to get adequate healthcare, to the nurses working two jobs in Nigeria. We get it." ]
            , paragraph
                [ Font.center
                , pt 12
                , rsJustify
                , lineHeight 1.6
                ]
                [ text "We consider ourselves agents of change for the future of nursing. Flint couples technology with insights and expertise — a winning combination." ]
            , paragraph
                [ Font.center
                , pt 12
                , rsJustify
                , lineHeight 1.6
                ]
                [ text "Flint is venture capital backed by the same people who funded the likes of AirBnB, Doordash & Instacart. Trust us to build long lasting solutions and partnerships within the healthcare industry." ]
            ]
        ]


header : Device.Device -> Model -> Element Msg
header device model =
    let
        bg =
            [ css "background" "#FFDCC9"
            , css "background" "linear-gradient(281.5deg, #FFDCC9 -0.43%, #C8BCC7 8.22%, #8284AF 27.81%, #6E74A9 52.4%, #6359A1 82.46%)"
            ]

        menu =
            [ ( "Partnerships", Partnerships ), ( "Nurse Careers", NurseCareers "" ) ]

        blobSrc =
            "/static/images/header-blob-blue.svg"

        title =
            "Freedom. Equality. Quality."

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

        -- responsive size
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
            [ -- GAP
              case device of
                Device.Phone _ ->
                    row [ wf, height <| fillPortion 4 ] [ Element.none ]

                _ ->
                    row [ wf, height <| fillPortion 4 ]
                        [ -- MENU
                          row [ wf ]
                            [ -- GAP
                              row [ width <| fillPortion 7 ] []

                            -- MENU
                            , row
                                [ width <| fillPortion 4
                                , spacing 32
                                , Font.color palette.white
                                , Font.letterSpacing 2
                                , Font.size 14
                                ]
                                [ row [ alignRight, spacingXY 36 0 ]
                                    (List.map (el (wf :: Styles.menu) << link) menu)
                                ]

                            -- GAP
                            , row [ width <| fillPortion 2 ] []
                            ]
                        ]

            -- TITLE
            , row [ wf, height <| fillPortion 8 ]
                [ row ([ wf, centerX, Font.size rs.titleFontSize ] ++ Styles.title)
                    [ paragraph [ Font.center, Font.size rs.titleFontSize ] [ text title ] ]
                ]

            -- GAP
            , case device of
                Device.Phone _ ->
                    Element.none

                _ ->
                    row [ wf, height <| fillPortion 2 ] []
            ]
        ]
