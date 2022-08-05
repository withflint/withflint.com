module Partnerships.View exposing (view)

import Device exposing (Device(..))
import Element
    exposing
        ( Element
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
        , link
        , mouseOver
        , padding
        , paddingEach
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
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes as HtmlAttr
import Layout exposing (Layout, footer, phoneMenu)
import Partnerships.Types exposing (Model, Msg(..))
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, css, hf, lineHeight, palette, pt, wf)


view : Device -> Model -> Layout Msg
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
                (desktopView device model
                    ++ footer.phone
                )
            ]
    , tablet =
        render <|
            [ column
                [ wf
                ]
                (desktopView device model
                    ++ footer.tablet
                )
            ]
    , desktop =
        render <|
            [ column
                [ wf
                ]
                (desktopView device model
                    ++ footer.desktop
                )
            ]
    }


desktopView : Device -> Model -> List (Element Msg)
desktopView device model =
    let
        sectionBg =
            [ css "background" "#DAE9FF"
            , css "background" "linear-gradient(180deg, #FFFBF8 0%, #DAE9FF 99.99%, #DAE9FF 100%)"
            ]
    in
    [ column
        [ Background.color colors.cremeDark
        , wf
        , hf
        , Font.family [ Font.typeface "Inter" ]
        ]
        [ header
            device
            model
            "Recreate the way you hire nurses"
            [ ( "Partnerships", Partnerships ), ( "Nurse Careers", NurseCareers "" ) ]
        , row (wf :: sectionBg)
            [ row [ width <| fillPortion 2 ] [ Element.none ]
            , column [ width <| fillPortion 8 ] [ section0 device ]
            , row [ width <| fillPortion 2 ] [ Element.none ]
            ]
        , partners device
        ]
    ]


section0 : Device.Device -> Element msg
section0 device =
    let
        titleStyle =
            [ Font.center
            , Font.size 28
            , Font.semiBold
            , Font.color palette.primary
            ]

        subHeading =
            [ Font.size 18
            , Font.semiBold
            , Font.color palette.primary
            ]

        rsJustify =
            case device of
                Device.Phone _ ->
                    Font.center

                _ ->
                    Font.justify

        btn =
            [ Border.roundEach { topLeft = 16, topRight = 0, bottomRight = 16, bottomLeft = 0 }
            , Border.color palette.primary
            , Border.width 1
            , padding 10
            , Font.color palette.white
            , Background.color palette.primary
            , Font.semiBold
            , Font.size 16
            , paddingEach { top = 10, right = 19, bottom = 10, left = 22 }
            , Font.regular
            , mouseOver
                [ Font.color colors.cremeLight
                , Background.color colors.carminePink
                , Border.color colors.carminePink
                ]
            ]
    in
    column [ wf, centerX, paddingXY 0 48, spacingXY 0 48 ]
        [ column [ spacingXY 0 12, centerX ]
            [ paragraph titleStyle
                [ text "America is short on nurses." ]
            , paragraph titleStyle
                [ text "We find exceptional international nurses to fill your vacancies." ]
            ]
        , paragraph [ Font.letterSpacing 2, pt 12, rsJustify, lineHeight 1.6 ]
            [ text "Hiring internationally is complicated and risky. Flint makes it simple and predictable. By sourcing in 190 countries, we can service the needs of your facility. Our technology platform enables us to overcome immigration and hiring variables that others cannot. This means fast turnaround." ]
        , column [ spacingXY 0 12 ]
            [ paragraph subHeading [ text "Recruit enthusiastic nurses with experience and know-how" ]
            , paragraph subHeading [ text "Decrease your staffing costs by over 50%" ]
            , paragraph subHeading [ text "Build a long-term recruitment channel. Never be short of nurses again." ]
            ]

        -- Btn
        , column [ centerX, spacingXY 0 16 ]
            [ el [ wf ]
                (link
                    (centerY :: centerX :: wf :: Font.size 15 :: btn)
                    { url = "https://calendly.com/d/d4h-b72-6y9/flint-introduction?month=2022-07"
                    , label = paragraph [ Font.center ] [ text <| "Partner with Flint" ]
                    }
                )
            , el [ wf ]
                (link
                    [ centerY, centerX, wf, Font.size 15 ]
                    { url = "https://calendly.com/d/d4h-b72-6y9/flint-introduction?month=2022-07"
                    , label = paragraph [ Font.center, Font.underline, Font.semiBold, Font.color palette.primary ] [ text <| "Contact Us" ]
                    }
                )
            ]
        ]


partners : Device -> Element msg
partners device =
    let
        bgBlue =
            [ css "background" "#5C4B92"
            , css "background" "linear-gradient(90deg, #50417F 0%, #5C4B92 100%)"
            ]
    in
    wrappedRow [ wf ]
        [ column
            ([ width <| fillPortion 6
             , paddingXY 28 96
             , hf
             , spacingXY 0 40
             ]
                ++ bgBlue
            )
            [ Element.image [ centerX, centerY ] { src = "/static/images/cgfns-logo.png", description = "CGFNS International" }
            , row [ centerX, centerY, spacingXY 12 0 ]
                [ Element.image [] { src = "/static/images/jsa-logo.png", description = "JSA" }
                , column [ wf, Font.color colors.white1 ]
                    [ paragraph [] [ text "Josef Silny & Associates, Inc." ]
                    , paragraph [] [ text "International Education Consultants" ]
                    ]
                ]
            ]
        , column [ width <| fillPortion 6, Background.color palette.skyBlue, hf, paddingXY 28 100, spacingXY 0 24, centerX, hf ]
            [ paragraph [ Font.center, Font.size 28, Font.color palette.primary, centerY ] [ text "We partner with the most trusted names in the business" ]
            , paragraph [ centerY, centerX, Font.center, width (fill |> Element.maximum 600), lineHeight 1.6 ] [ text "Flint holds high standards and invest in quality nurses by partnering with the most trusted names in immigration services" ]
            ]
        ]


header : Device -> Model -> String -> List ( String, Page ) -> Element Msg
header device model title menu =
    let
        bg =
            [ css "background" "rgb(68,55,109)"
            , css "background" "linear-gradient(281.17deg, #A7C8F9 -8.91%, #8494C7 12.48%, #6E74A9 42.43%, #626297 82.36%)"
            ]

        blob =
            row [ css "position" "relative" ]
                [ row
                    [ alignTop
                    , css "position" "relative"
                    , width (px 275)
                    , height (px 139)
                    ]
                    [ html <|
                        Html.img
                            [ HtmlAttr.src "/static/images/header-blob-blue.svg"
                            , HtmlAttr.style "width" "100%"
                            ]
                            []
                    ]
                , logo
                ]

        -- responsive size
        rs =
            case device of
                Phone _ ->
                    { titleFontSize = 36
                    }

                Tablet _ ->
                    { titleFontSize = 32
                    }

                Desktop _ ->
                    { titleFontSize = 44
                    }

                NotSet ->
                    { titleFontSize = 0
                    }

        logo =
            row
                [ css "position" "absolute"
                , css "left" "44px"
                , css "top" "20px"
                , css "z-index" "100"
                ]
                [ Element.link
                    []
                    { url = toPath Home
                    , label =
                        Element.image
                            [ width (px 110), height (px 54) ]
                            { src = "/static/images/logo.svg?new", description = "Flint" }
                    }
                ]

        link : ( String, Page ) -> Element msg
        link ( label, page ) =
            Element.link
                []
                { url = toPath page
                , label =
                    el [ Font.center ] (text label)
                }

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
                Phone _ ->
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
                    [ paragraph [ paddingXY 24 0, Font.center, Font.size rs.titleFontSize ] [ text title ] ]
                ]

            -- GAP
            , case device of
                Phone _ ->
                    Element.none

                _ ->
                    row [ wf, height <| fillPortion 2 ] []
            ]
        ]
