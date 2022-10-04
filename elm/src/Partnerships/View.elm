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
        , rgb255
        , row
        , spaceEvenly
        , spacing
        , spacingXY
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Framework.Heading as Heading
import Html
import Html.Attributes as HtmlAttr
import Layout exposing (Layout, footer, phoneMenu, topMenu)
import Partnerships.Types exposing (Model, Msg(..))
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, css, hf, lineHeight, maxW, paddingE, palette, wf, wp)


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


setResponsiveVal : Device.Device -> { phone : a, tablet : a, desktop : a, notSet : a } -> a
setResponsiveVal device { phone, desktop, tablet, notSet } =
    case device of
        Device.Phone _ ->
            phone

        Device.Desktop _ ->
            desktop

        Device.Tablet _ ->
            tablet

        Device.NotSet ->
            notSet


desktopView : Device -> Model -> List (Element Msg)
desktopView device model =
    let
        fillPortionVal =
            setResponsiveVal device { phone = 0, desktop = 2, tablet = 2, notSet = 0 }
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
            topMenu
        , row
            [ wf ]
            [ row [ width <| fillPortion fillPortionVal ] [ Element.none ]
            , column [ width <| fillPortion 8 ] [ section0 device ]
            , row [ width <| fillPortion fillPortionVal ] [ Element.none ]
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
            , Font.color colors.primary
            ]

        btn =
            [ Border.rounded 8
            , Border.color colors.primary
            , Border.width 1
            , padding 10
            , Font.color colors.white
            , Background.color colors.primary
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
                [ text "Flint injects top international nurses into healthcare facilities nationwide." ]
            ]
        , paragraph [ paddingE 12 18 0 18, Font.center, lineHeight 1.6 ]
            [ text "Hiring internationally is complicated and risky. Flint makes it simple and predictable. By sourcing in 190 countries, we can service the needs of your facility. Our technology enables us to overcome immigration and hiring variables that others cannot. This means fast turnaround." ]
        , column
            [ centerX, spacingXY 0 24 ]
            [ valueCard device experiencedNurses
            , valueCard device savings
            , valueCard device neverBeShortNurses
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
                    , label = paragraph [ Font.center, Font.underline, Font.semiBold, Font.color colors.primary ] [ text <| "Contact Us" ]
                    }
                )
            ]
        ]


experiencedNurses : { iconUrl : String, iconDesc : String, heading : String, desc : String }
experiencedNurses =
    { iconUrl = "/static/images/partnerships-nurse.svg"
    , iconDesc = "Flint - Experienced Nurses"
    , heading = "Recruit experienced nurses who are committed for 3+ years"
    , desc = "Our nurses have 3-10 years of clinical experience, and are looking to build a long term career at the right facility."
    }


savings : { iconUrl : String, iconDesc : String, heading : String, desc : String }
savings =
    { iconUrl = "/static/images/partnerships-savings.svg"
    , iconDesc = "Flint - Save costs by partnerting with Flint"
    , heading = "Save millions by replacing agency with your own staff"
    , desc = "On average, we help facilities save 50% in staffing costs compared with agencies. For every 10 nurses sourced through Flint, expect to save $1M/year."
    }


neverBeShortNurses : { iconUrl : String, iconDesc : String, heading : String, desc : String }
neverBeShortNurses =
    { iconUrl = "static/images/partnerships-never-short-nurses.svg"
    , iconDesc = "Flint - Never be short on nurses again"
    , heading = "Never be short on nurses again"
    , desc = "It's not just about today, it's about tomorrow. We work with you to develop a sustainable recruiting pipeline that you can count on for years to come."
    }


valueCard : Device.Device -> { iconUrl : String, iconDesc : String, heading : String, desc : String } -> Element msg
valueCard device { iconUrl, iconDesc, heading, desc } =
    let
        responsiveDiv =
            case device of
                Device.Phone _ ->
                    column

                Device.Tablet _ ->
                    row

                Device.Desktop _ ->
                    row

                Device.NotSet ->
                    row
    in
    responsiveDiv [ spacingXY 12 12, Background.color colors.cremeLight, Border.rounded 12, padding 24 ]
        [ row [ centerX ]
            [ Element.image [ centerX, width (px 72), height (px 87) ] { src = iconUrl, description = iconDesc }
            ]
        , column [ maxW 550, spacingXY 0 12, padding 12 ]
            [ paragraph [ css "width" "100%", Font.color colors.primary, Font.bold ] [ text heading ]
            , paragraph [ css "width" "100%", Font.color (rgb255 25 21 41) ] [ text desc ]
            ]
        ]


partners : Device -> Element msg
partners device =
    let
        bgBlue =
            [ css "background" "#5C4B92"
            , css "background" "linear-gradient(90deg, #50417F 0%, #5C4B92 100%)"
            ]

        rsPortion =
            case device of
                Device.Phone _ ->
                    { row1 = wf
                    , row2 = wf
                    , row3 = wf
                    , spacing = spacingXY 0 32
                    , bg = Background.color colors.white
                    }

                Device.Desktop _ ->
                    { row1 = wp 2
                    , row2 = wp 8
                    , row3 = wp 2
                    , spacing = spaceEvenly
                    , bg = Background.color palette.cremeLight
                    }

                Device.Tablet _ ->
                    { row1 = wp 0
                    , row2 = wp 12
                    , row3 = wp 0
                    , spacing = spaceEvenly
                    , bg = Background.color palette.cremeLight
                    }

                Device.NotSet ->
                    { row1 = wp 1
                    , row2 = wp 10
                    , row3 = wp 1
                    , spacing = spaceEvenly
                    , bg = Background.color palette.cremeLight
                    }
    in
    column [ wf ]
        [ wrappedRow
            ([ wf
             , hf
             ]
                ++ bgBlue
            )
            [ row [ rsPortion.row1 ] []
            , wrappedRow
                [ rsPortion.row2
                , paddingXY 32 128
                , rsPortion.spacing
                ]
                [ column [ spacingXY 0 24, alignTop ]
                    [ row [ width (px 210), height (px 75) ]
                        [ Element.image [ centerX, css "width" "100%" ] { src = "/static/images/cgfns-logo.svg", description = "CGFNS International" }
                        ]
                    , column [ spacingXY 12 12 ]
                        [ row [ width (px 95), height (px 83) ]
                            [ Element.image [ centerX, css "width" "100%" ] { src = "/static/images/jsa-logo.svg", description = "JSA" }
                            ]
                        , column [ wf, Font.color colors.white1, Font.size 12 ]
                            [ paragraph [] [ text "Josef Silny & Associates, Inc." ]
                            , paragraph [] [ text "International Education Consultants" ]
                            ]
                        ]
                    ]
                , column [ spacingXY 0 48, alignTop ]
                    [ row [ width (px 224), height (px 95) ]
                        [ Element.image [ centerX, css "width" "100%" ] { src = "/static/images/hca-logo.svg", description = "HCA Healthcare" }
                        ]
                    , row [ width (px 264), height (px 65) ]
                        [ Element.image [ centerX, css "width" "100%" ] { src = "/static/images/medall-logo.svg", description = "MedAll" }
                        ]
                    ]
                , column [ spacingXY 0 48 ]
                    [ row [ width (px 198), height (px 52) ]
                        [ Element.image [ centerX, css "width" "100%" ] { src = "/static/images/ringmd-logo.svg", description = "RingMd" }
                        ]
                    , row [ width (px 264), height (px 65) ]
                        [ Element.image [ centerX, css "width" "100%" ] { src = "/static/images/learn-with-nurses-logo.svg", description = "Learn with Nurses" }
                        ]
                    ]
                ]
            , row [ rsPortion.row3 ] []
            ]

        -- ##### We partner with #####
        , column [ wf, rsPortion.bg, hf, paddingXY 28 100, spacingXY 0 24, centerX, hf ]
            [ paragraph [ Font.center, Font.size 28, Font.color colors.primary, centerY ] [ text "We partner with the most trusted names in the business." ]
            , paragraph [ centerY, centerX, Font.center, width (fill |> Element.maximum 600), lineHeight 1.6 ] [ text "Flint's industry partnerships mean the highest standards in nurse quality and competency." ]
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
                                , Font.color colors.white
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
                [ el ([ wf, centerX, Font.size rs.titleFontSize ] ++ Styles.title ++ Heading.h1)
                    (paragraph [ paddingXY 24 0, Font.center, Font.size rs.titleFontSize ] [ text title ])
                ]

            -- GAP
            , case device of
                Phone _ ->
                    Element.none

                _ ->
                    row [ wf, height <| fillPortion 2 ] []
            ]
        ]
