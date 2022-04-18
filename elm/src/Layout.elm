module Layout exposing (Layout, footer, header, layout, menu)

import Device exposing (Device(..))
import Element
    exposing
        ( Element
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , image
        , link
        , newTabLink
        , padding
        , paddingEach
        , paddingXY
        , px
        , row
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Font as Font exposing (underline)
import FaqNurses.Types exposing (Faq)
import Router.Routes exposing (Page(..), toPath)
import Styles


type alias Layout msg =
    { phone : List (Element msg)
    , tablet : List (Element msg)
    , desktop : List (Element msg)
    }


layout : Device -> Layout msg -> Element msg
layout device views =
    case device of
        Phone ->
            column
                [ height fill
                , width fill
                , centerX
                , alignTop
                ]
                views.phone

        Tablet ->
            column
                [ height fill
                , width fill
                , centerX
                , alignTop
                ]
                views.tablet

        _ ->
            column
                [ height fill
                , width fill
                , centerX
                , alignTop
                ]
                views.desktop


header : Layout msg
header =
    let
        default : List (Element msg)
        default =
            [ row [ width fill ]
                [ column [ width fill ]
                    [ Element.link
                        []
                        { url = toPath Home
                        , label = Element.image [ centerY, alignLeft, width (px 100), height (px 50) ] { src = "/static/images/logo.svg", description = "Flint" }
                        }
                    ]
                , column [ width fill, alignRight ]
                    [ column (width fill :: Styles.paragraph)
                        [ row [ spacingXY 30 0, alignRight ]
                            (menu |> List.map (\( path, label ) -> row [] [ link [ padding 5 ] { url = toPath path, label = text label } ]))
                        ]
                    ]
                ]
            ]
    in
    { phone =
        [ row []
            [ Element.link
                []
                { url = toPath Home
                , label = Element.image [ centerY, alignLeft, width (px 100), height (px 50) ] { src = "/static/images/logo.svg", description = "Flint" }
                }
            ]
        ]
    , tablet = default
    , desktop = default
    }


footer : Layout msg
footer =
    let
        default : List (Element msg)
        default =
            [ row [ height fill ] []
            , row [ width fill, paddingEach { top = 100, bottom = 20, left = 0, right = 0 } ]
                [ column [ width fill ]
                    [ row
                        [ width fill
                        ]
                        [ image [ width (px 80), height (px 50) ]
                            { src = "/static/images/logo.svg"
                            , description = "Flint"
                            }
                        ]
                    , row [ width fill, Font.size 10, paddingXY 0 10 ]
                        [ column [ spacing 20 ] [ text "© 2021 Flint, all rights reserved", newTabLink [ underline ] { url = "/privacy", label = text "Privacy Policy" } ] ]
                    ]
                , column [ width fill, alignBottom, alignRight ]
                    [ row [ spacingXY 60 0, centerX, alignRight ]
                        [ row []
                            [ newTabLink
                                []
                                { url = "https://www.ycombinator.com/companies/flint"
                                , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/YC_logo.svg", description = "Flint" }
                                }
                            ]
                        , row []
                            [ newTabLink
                                []
                                { url = "https://github.com/withflint"
                                , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/github_logo.svg", description = "Flint" }
                                }
                            ]
                        , row []
                            [ newTabLink
                                []
                                { url = "https://www.linkedin.com/company/withflint/"
                                , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/linkedin-icon-2.svg", description = "Flint" }
                                }
                            ]
                        ]
                    ]
                ]
            ]
    in
    { phone =
        [ row [ width fill, centerX, paddingXY 0 50 ]
            [ column ([ centerX, width fill, spacingXY 50 20, centerX, Font.size 15 ] ++ Styles.paragraph)
                (menu |> List.map (\( path, label ) -> row [ width fill, centerX ] [ link [ centerX, padding 5 ] { url = toPath path, label = text label } ]))
            ]
        , row [ width fill, centerX ]
            [ column [ width fill, alignBottom ]
                [ row [ spacingXY 60 0, centerX ]
                    [ row []
                        [ newTabLink
                            []
                            { url = "https://www.ycombinator.com/companies/flint"
                            , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/YC_logo.svg", description = "Flint" }
                            }
                        ]
                    , row []
                        [ newTabLink
                            []
                            { url = "https://github.com/withflint"
                            , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/github_logo.svg", description = "Flint" }
                            }
                        ]
                    , row []
                        [ newTabLink
                            []
                            { url = "https://www.linkedin.com/company/withflint/"
                            , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/linkedin-icon-2.svg", description = "Flint" }
                            }
                        ]
                    ]
                ]
            ]
        , row
            [ width fill, paddingEach { top = 30, bottom = 10, left = 0, right = 0 } ]
            [ image [ width (px 50), height (px 30), centerX ]
                { src = "/static/images/logo.svg"
                , description = "Flint"
                }
            ]
        , row [ centerX, width fill, Font.size 10, paddingXY 0 10 ]
            [ el [ centerX ] <| column [ spacing 20, centerX ] [ text "© 2021 Flint, all rights reserved", newTabLink [ underline, centerX ] { url = "/privacy", label = text "Privacy Policy" } ] ]
        ]
    , tablet = default
    , desktop = default
    }


menu : List ( Page, String )
menu =
    [ ( Home, "Home" )
    , ( HealthCare "", "Health Care Jobs" )
    , ( FaqNurses, "FAQ" )
    , ( Contact, "Contact" )
    , ( Jobs "", "Jobs" )
    ]
