module Layout exposing (HeaderConfig, HeaderIconBgColor(..), Layout, footer, footer_, header, layout, menu, phoneMenu, topMenu)

import Device exposing (Device(..))
import Element
    exposing
        ( Attribute
        , Element
        , alignBottom
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
        , htmlAttribute
        , image
        , inFront
        , moveDown
        , moveLeft
        , newTabLink
        , none
        , paddingEach
        , paddingXY
        , paragraph
        , px
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
import Element.Font as Font exposing (underline)
import Element.Input as Input
import Html.Attributes
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, css, hf, palette, pt, wf)


menu : List ( String, Page )
menu =
    [ ( "Partnerships", Partnerships )
    , ( "Nurse Careers", NurseCareers "" )
    , ( "FAQ", FaqNurses )
    , ( "About", About )
    , ( "Blog", Blog "" )
    , ( "Join", JoinTheTeam "" )
    ]


topMenu : List ( String, Page )
topMenu =
    [ ( "Partnerships", Partnerships ), ( "Nurse Careers", NurseCareers "" ), ( "Blog", Blog "" ), ( "About", About ) ]


type alias Layout msg =
    { phone : List (Element msg)
    , tablet : List (Element msg)
    , desktop : List (Element msg)
    }


layout : Device -> Layout msg -> Element msg
layout device views =
    case device of
        Phone _ ->
            column
                [ wf
                , hf
                , Background.color colors.cremeLight
                ]
                views.phone

        Tablet _ ->
            column
                [ wf
                , hf
                , Background.color colors.cremeDark
                ]
                views.tablet

        _ ->
            column
                [ wf
                , hf
                ]
                [ column
                    [ wf
                    , hf
                    , Background.color colors.cremeDark
                    ]
                    views.desktop
                ]


address : { street : String, city : String, country : String }
address =
    { street = "2261 Market St"
    , city = "San Francisco, CA 94114"
    , country = "USA"
    }


showAddress : Element msg
showAddress =
    column [ spacingXY 0 6 ]
        [ paragraph [] [ text address.street ]
        , paragraph [] [ text address.city ]
        , paragraph [] [ text address.country ]
        ]


type alias HeaderConfig msg =
    { navigations : List ( Page, String )
    , title : String
    , device : Device
    , attributes : List (Attribute msg)
    , headerIconBg : HeaderIconBgColor
    }


type HeaderIconBgColor
    = HeaderIconBgBeige
    | HeaderIconBgBlue


header : HeaderConfig msg -> Element msg
header { navigations, title, device, attributes, headerIconBg } =
    let
        logo =
            row
                [ htmlAttribute <| Html.Attributes.style "position" "absolute"
                , htmlAttribute <| Html.Attributes.style "top" "0"
                , htmlAttribute <| Html.Attributes.style "left" "0"
                , inFront <|
                    Element.link
                        [ width fill
                        , moveLeft 30
                        , moveDown 15
                        ]
                        { url = toPath Home
                        , label =
                            Element.image
                                [ width (px 110), height (px 54) ]
                                { src = "/static/images/logo.svg?new", description = "flint-logo" }
                        }
                ]
                [ image
                    [ alignTop, width (px 275), height (px 139) ]
                    { src =
                        case headerIconBg of
                            HeaderIconBgBeige ->
                                "/static/images/header-blob-beige.svg"

                            HeaderIconBgBlue ->
                                "/static/images/header-blob-blue.svg"
                    , description = "background"
                    }
                ]

        heading =
            row [ width fill, paddingEach { top = 138, bottom = 0, left = 0, right = 0 } ]
                [ paragraph
                    [ width fill
                    , centerX
                    , Font.center
                    , Font.family [ Font.typeface "Inter" ]
                    , Font.color colors.white
                    , Font.bold
                    , Font.size <|
                        case device of
                            Device.Phone _ ->
                                36

                            Device.Tablet _ ->
                                32

                            Device.Desktop _ ->
                                44

                            Device.NotSet ->
                                0
                    ]
                    [ text title ]
                ]

        navigation =
            case device of
                Device.Phone _ ->
                    none

                Device.Tablet _ ->
                    none

                _ ->
                    let
                        link ( page, label ) =
                            Element.link
                                [ htmlAttribute <| Html.Attributes.class "menu"
                                , htmlAttribute <| Html.Attributes.style "z-index" "100"
                                ]
                                { url = toPath page
                                , label = el [ Font.center ] (text label)
                                }
                    in
                    row
                        [ spacingXY 52 0
                        , Font.color colors.white
                        , Font.letterSpacing 2
                        , Font.size 14
                        , Border.rounded 6
                        , Font.family [ Font.typeface "Inter" ]
                        , Font.semiBold
                        , alignRight
                        , paddingEach { top = 33, bottom = 0, left = 0, right = 0 }
                        ]
                        (List.map link navigations)

        margin n =
            el [ width <| fillPortion n ] none
    in
    row
        ([ width fill
         , height (px 280)
         , inFront heading
         , inFront logo
         ]
            ++ attributes
        )
        [ margin 7
        , column
            [ width (fillPortion 4)
            , alignTop
            ]
            [ navigation
            ]
        , margin 2
        ]


footer_ : Device -> List (Element msg)
footer_ device =
    case device of
        Device.Phone _ ->
            footer.phone

        Device.Tablet _ ->
            footer.tablet

        Device.Desktop _ ->
            footer.desktop

        Device.NotSet ->
            []



-- 2023-04-10 Jimmy
-- Footer should just be a function takes a device and choose the proper layout by pattern matching. Now
-- the pattern matching needs to be done in all call sites. I keep it here for now so I don't change to much
-- code, but we need to stop using it and use `footer_` instead.


footer : Layout msg
footer =
    let
        privacy =
            row [ Font.size 10 ]
                [ newTabLink [ underline ] { url = "/privacy", label = text "Privacy Policy" }
                ]

        default : List (Element msg)
        default =
            [ row
                [ wf
                , hf
                , css "position" "relative"
                , Font.size 12
                ]
                [ row [ css "position" "absolute", css "top" "0", css "left" "-12px" ]
                    [ image [ width (px 164), height (px 114) ]
                        { src = "/static/images/footer-blob-left.svg"
                        , description = ""
                        }
                    ]
                , column [ hf, wf ]
                    [ row [ hf, wf ]
                        [ wrappedRow [ alignBottom, wf, paddingXY 0 48 ]
                            [ row [ width (px 200) ] []
                            , row [ wf, spaceEvenly ]
                                [ column [ hf, centerY, wf ]
                                    [ Element.link
                                        []
                                        { url = toPath Home
                                        , label =
                                            image [ width (px 80), height (px 50) ]
                                                { src = "/static/images/logo.svg?new"
                                                , description = "Flint Logo"
                                                }
                                        }
                                    , column [ pt 20 ] [ showAddress ]
                                    ]
                                , column [ hf, wf, spacingXY 0 12 ] <|
                                    toLinks menu
                                , column [ hf, centerY, wf, spacingXY 0 40 ]
                                    [ column [ alignRight, spacing 40 ]
                                        [ column [ hf, Font.color colors.primary, spacingXY 0 12 ]
                                            [ Element.paragraph [] [ text "Partnerships" ]
                                            , column [ spacingXY 0 6 ]
                                                [ Element.paragraph [] [ text "healthcare@withflint.com" ]
                                                , Element.paragraph [] [ text "+1 (844) 677-1180" ]
                                                ]
                                            ]
                                        , column [ hf, Font.color colors.primary, spacingXY 0 12 ]
                                            [ Element.paragraph [] [ text "Nurse Success" ]
                                            , column [ spacingXY 0 6 ]
                                                [ Element.paragraph [] [ text "success@withflint.com" ]
                                                , Element.paragraph [] [ text "+1 (844) 677-1180" ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        , row [ width (px 200) ] []
                        ]
                    ]
                , row
                    [ css "position" "absolute"
                    , css "bottom" "0px"
                    , css "right" "0px"
                    ]
                    [ image [ width (px 140), height (px 100) ]
                        { src = "/static/images/blob-right.svg"
                        , description = ""
                        }
                    ]
                ]
            , row [ wf, Background.color palette.cremeLighter, paddingXY 88 24, centerX ]
                [ row [ wf, spaceEvenly ]
                    [ row [ spaceEvenly ] [ privacy ]
                    , row [ spaceEvenly, wf ] [ paragraph [ Font.center, Font.size 10 ] [ text "© 2022 Flint, all rights reserved" ] ]
                    , row [ spaceEvenly, spacingXY 36 0 ]
                        [ row []
                            [ newTabLink
                                []
                                { url = "https://www.ycombinator.com/companies/flint"
                                , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/YC_logo.svg?new", description = "Flint Y Combinator" }
                                }
                            ]
                        , row []
                            [ newTabLink
                                []
                                { url = "https://github.com/withflint"
                                , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/github_logo.svg?new", description = "Flint GitHub" }
                                }
                            ]
                        , row []
                            [ newTabLink
                                []
                                { url = "https://www.linkedin.com/company/withflint/"
                                , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/linkedin-icon-2.svg?new", description = "Flint Linkedin" }
                                }
                            ]
                        , row []
                            [ newTabLink
                                []
                                { url = "https://instagram.com/withflint"
                                , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/instagram.svg?new", description = "Flint Instagram" }
                                }
                            ]
                        , row []
                            [ newTabLink
                                []
                                { url = "https://facebook.com/withflint"
                                , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/facebook.svg?new", description = "Flint Facebook" }
                                }
                            ]
                        ]
                    ]
                ]
            ]
    in
    { phone =
        [ column [ wf, hf, paddingXY 0 48, Background.color palette.cremeLight, spacingXY 0 32 ]
            [ column [ centerX ]
                [ Element.image [ width (px 90), height (px 34) ] { src = "/static/images/logo.svg?new", description = "Flint" }
                ]
            , column [ centerX, pt 12, Font.color colors.primary, spacing 10, Font.center ] <| toLinks menu
            , column [ Font.center, centerX, spacingXY 0 16 ]
                [ column [ hf, spacingXY 0 12 ]
                    [ Element.paragraph [] [ text "Partnerships" ]
                    , column [ spacingXY 0 6 ]
                        [ Element.paragraph [] [ text "healthcare@withflint.com" ]
                        , Element.paragraph [] [ text "+1 (844) 677-1180" ]
                        ]
                    ]
                , column [ hf, spacingXY 0 12, centerX ]
                    [ Element.paragraph [] [ text "Nurse Success" ]
                    , column [ spacingXY 0 6 ]
                        [ Element.paragraph [] [ text "success@withflint.com" ]
                        , Element.paragraph [] [ text "+1 (844) 677-1180" ]
                        ]
                    ]
                , column [ pt 12, centerX ]
                    [ showAddress ]
                ]
            ]

        -- social and privacy
        , column [ wf, paddingXY 12 36, paddingEach { top = 36, bottom = 192, left = 12, right = 12 }, Background.color palette.cremeLighter, spacingXY 0 32 ]
            [ row [ centerX, spacingXY 16 0 ]
                [ row []
                    [ newTabLink
                        []
                        { url = "https://www.ycombinator.com/companies/flint"
                        , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/YC_logo.svg?new", description = "Flint Y Combinator" }
                        }
                    ]
                , row []
                    [ newTabLink
                        []
                        { url = "https://github.com/withflint"
                        , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/github_logo.svg?new", description = "Flint GitHub" }
                        }
                    ]
                , row []
                    [ newTabLink
                        []
                        { url = "https://www.linkedin.com/company/withflint/"
                        , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/linkedin-icon-2.svg?new", description = "Flint Linkedin" }
                        }
                    ]
                , row []
                    [ newTabLink
                        []
                        { url = "https://instagram.com/withflint"
                        , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/instagram.svg?new", description = "Flint Instagram" }
                        }
                    ]
                , row []
                    [ newTabLink
                        []
                        { url = "https://facebook.com/withflint"
                        , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/facebook.svg?new", description = "Flint Facebook" }
                        }
                    ]
                ]
            , row [ spaceEvenly, wf ] [ paragraph [ Font.center, Font.size 10 ] [ text "© 2022 Flint, all rights reserved" ] ]
            , row [ centerX ] [ privacy ]
            ]
        ]
    , tablet = default
    , desktop = default
    }


phoneMenu : msg -> Bool -> Element msg
phoneMenu msg isMenuVisible =
    let
        hamburger =
            column [ spacingXY 0 3 ]
                [ row [ width (px 25), height (px 4), Background.color colors.primary, Border.rounded 4 ] []
                , row [ width (px 25), height (px 4), Background.color colors.primary, Border.rounded 4 ] []
                , row [ width (px 25), height (px 4), Background.color colors.primary, Border.rounded 4 ] []
                ]

        hamburgerIcon =
            row
                [ css "position" "fixed"
                , css "right" "30px"
                , css "top" "30px"
                , css "z-index" "200"
                ]
                [ Input.button []
                    { onPress = Just msg
                    , label = hamburger
                    }
                ]

        bg =
            [ css "background" "#6359A1"
            , css "background" "linear-gradient(162.39deg, #5D3968 0%, #6359A1 100%)"
            ]
    in
    if isMenuVisible then
        column
            [ wf
            , hf
            ]
            [ column ([ wf, hf, Font.color colors.white ] ++ bg)
                [ column [ wf, height <| fillPortion 2 ]
                    [ row [ alignRight, centerY ]
                        [ Input.button []
                            { onPress = Just msg
                            , label =
                                paragraph [ paddingEach { top = 44, right = 56, bottom = 32, left = 32 } ] [ text "CLOSE" ]
                            }
                        ]
                    ]
                , column [ wf, height <| fillPortion 1 ] []
                , column [ wf, height <| fillPortion 9 ]
                    [ column [ spacingXY 0 42, alignTop, centerX, Font.size 28 ] <| toLinks <| menu
                    ]
                ]
            ]

    else
        hamburgerIcon


toLinks : List ( String, Page ) -> List (Element msg)
toLinks m =
    let
        link ( label, page ) =
            Element.link
                [ wf ]
                { url = toPath page
                , label = Element.el [ Font.center, wf ] (text label)
                }
    in
    List.map link m
