module Partnerships.View exposing (..)

import Device exposing (Device(..))
import Element
    exposing
        ( Element
        , alignLeft
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
        , minimum
        , mouseOver
        , newTabLink
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , shrink
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
import Element.Input as Input
import Html
import Html.Attributes as HtmlAttr
import Layout exposing (Layout, footer, header)
import List exposing (maximum)
import Partnerships.Types exposing (Model)
import Styles exposing (buttons, colors, css, debug, heading, hf, palette, pl, pr, pt, wf)


view : Device -> Model -> Layout msg
view device _ =
    { phone =
        [ column
            [ -- centerX
              -- , width <| maximum 1500 fill
              -- ,
              wf
            , height fill

            -- , paddingXY 20 40
            ]
            -- header.phone
            -- ++
            --  phoneView
            --  phoneView device
            (desktopView device
                ++ footer.phone
            )
        ]
    , tablet =
        [ column
            [ -- centerX
              -- , width <| maximum 1500 fill
              wf

            -- , paddingXY 100 40
            ]
            -- header.tablet
            -- ++
            --  phoneView device
            (desktopView device
                ++ footer.tablet
            )
        ]
    , desktop =
        [ column
            [ --     centerX
              -- , width <| maximum 1500 fill
              -- ,
              --   height fill
              -- , paddingXY 100 40
              wf
            ]
            -- header.desktop
            -- ++
            (desktopView device
                ++ footer.desktop
            )
        ]
    }


desktopView : Device -> List (Element msg)
desktopView device =
    let
        sectionBg =
            [ htmlAttribute <| HtmlAttr.style "background" "#DAE9FF"
            , htmlAttribute <| HtmlAttr.style "background" "linear-gradient(180deg, #FFFBF8 0%, #DAE9FF 99.99%, #DAE9FF 100%)"
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
            "Recreate the way you hire nurses"
            [ "Partnership", "Nurse Careers", "Blog" ]
        , row ([ wf ] ++ sectionBg)
            [ row [ width <| fillPortion 2 ] [ Element.none ]
            , column [ width <| fillPortion 8 ] [ section0 ]
            , row [ width <| fillPortion 2 ] [ Element.none ]
            ]
        , partners device

        -- , paragraph [] [ text <| Debug.toString device ]
        ]
    ]


phoneView : Device -> List (Element msg)
phoneView device =
    [ column
        [ Background.color colors.cremeDark
        , wf
        , hf
        , Font.family [ Font.typeface "Inter" ]
        ]
        [ header
            device
            "Recreate the way you hire nurses"
            [ "Partnership", "Nurse Careers", "Blog" ]
        , row [ wf ]
            [ row [ width <| fillPortion 2 ] []
            , column [ width <| fillPortion 8 ] [ section0 ]
            , row [ width <| fillPortion 2 ] []
            ]

        -- , paragraph [] [ text <| Debug.toString device ]
        ]
    ]


section0 : Element msg
section0 =
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
    in
    column [ wf, centerX, paddingXY 0 48, spacingXY 0 48 ]
        [ column [ spacingXY 0 12, centerX ]
            [ paragraph titleStyle
                [ text "America is short nurses." ]
            , paragraph titleStyle
                [ text "We find the best international nurses to fill your vacancies" ]
            ]
        , paragraph [ Font.center, Font.letterSpacing 3, pt 12, Font.justify ]
            [ text "Hiring internationally is complicated and risky. Flint makes it simple and takes the risk away. \nWith 190 countries to work with we can service the needs of your facility. Our technology platform enables us to overcome immigration and hiring variables that others cannot. This means fast turnaround." ]
        , column [ spacingXY 0 12 ]
            [ paragraph subHeading [ text "Recruit incredible nurses with great experience" ]
            , paragraph subHeading [ text "Decrease your staffing costs by over 50%" ]
            , paragraph subHeading [ text "Build a long term recruitment funnel to never be short of nurses again" ]
            ]

        -- Btn
        , row [ centerX ]
            [ el [ wf ]
                (newTabLink
                    (centerY :: centerX :: wf :: Font.size 15 :: Styles.btn)
                    { url = "https://calendly.com/d/d4h-b72-6y9/flint-introduction?month=2022-07"
                    , label = paragraph [ Font.center ] [ text <| "See if your facility qualifies" ]
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
            , paragraph [ centerY, centerX, Font.center, width (fill |> Element.maximum 600) ] [ text "Flint holds high standards and invest in quality nurses by partnering with the most trusted names in immigration services" ]
            ]
        ]


header : Device -> String -> List String -> Element msg
header device title menu =
    let
        bg =
            [ css "background" "rgb(68,55,109)"
            , css "background" "linear-gradient(281.17deg, #A7C8F9 -8.91%, #8494C7 12.48%, #6E74A9 42.43%, #626297 82.36%)"
            ]

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
                Phone vp ->
                    { titleFontSize = 36
                    }

                Tablet vp ->
                    { titleFontSize = 32
                    }

                Desktop vp ->
                    { titleFontSize = 44
                    }

                -- 44
                NotSet ->
                    { titleFontSize = 0
                    }

        logo =
            row
                [ css "position" "absolute"
                , css "left" "44px"
                , css "top" "20px"
                ]
                [ Element.image
                    [ width (px 110), height (px 54) ]
                    { src = "/static/images/logo.svg?new", description = "Flint" }
                ]
    in
    row ([ wf, css "position" "relative" ] ++ bg)
        [ column [ css "position" "absolute", css "top" "0", css "left" "0" ]
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
                                [ width <| fillPortion 3

                                -- , spaceEvenly
                                , spacing 32
                                , Font.color palette.white
                                , Font.letterSpacing 2
                                , Font.size 14
                                ]
                                (List.map (el Styles.menu << text) menu)

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
                Phone vp ->
                    Element.none

                _ ->
                    row [ wf, height <| fillPortion 2 ] []
            ]
        ]
