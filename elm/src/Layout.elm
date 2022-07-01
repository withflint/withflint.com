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
        , spaceEvenly
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Font as Font exposing (underline)
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors)


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
                , Background.color colors.cremeLight
                ]
                views.phone

        Tablet ->
            column
                [ height fill
                , width fill
                , centerX
                , alignTop
                , Background.color colors.cremeLight
                ]
                views.tablet

        _ ->
            -- C changed
            column [ width fill, height fill ]
                [ column
                    [ height fill
                    , width fill
                    , centerX
                    , alignTop
                    , Background.color colors.cremeLight
                    ]
                    views.desktop
                , column [ width fill, height fill ]
                    footer.desktop
                ]


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
                        , label = Element.image [ centerY, alignLeft, width (px 100), height (px 50) ] { src = "/static/images/logo.svg?new", description = "Flint" }
                        }
                    ]
                , column [ width fill, alignRight ]
                    [ column (width fill :: Styles.paragraph)
                        [ row
                            [ spacingXY 30 0
                            , alignRight

                            -- C added color
                            -- , Font.color colors.blue1
                            ]
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
                , label = Element.image [ centerY, alignLeft, width (px 100), height (px 50) ] { src = "/static/images/logo.svg?new", description = "Flint" }
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
            [ row
                [ width fill
                , height fill
                , paddingEach { top = 60, bottom = 0, left = 0, right = 0 }
                , Background.color colors.cremeLight
                ]
                [ row [ width fill, height fill, Background.color colors.cremeDark ]
                    [ --row [ height fill ] []
                      row [ alignTop, alignLeft ]
                        [ image [ width (px 164), height (px 114) ]
                            { src = "/static/images/blob-1.svg"

                            -- default dimension 213x163
                            , description = ""
                            }
                        ]
                    , row
                        [ paddingEach { top = 60, bottom = 60, left = 0, right = 0 }

                        -- C added
                        , Background.color colors.cremeDark
                        , centerX
                        , spacingXY 600 0
                        ]
                        [ column []
                            [ row
                                []
                                [ image [ width (px 80), height (px 50) ]
                                    { src = "/static/images/logo.svg?new"
                                    , description = "Flint"
                                    }
                                ]
                            , row [ width fill, Font.size 10, paddingXY 0 10 ]
                                [ column [ spacing 20 ] [ text "© 2022 Flint, all rights reserved", newTabLink [ underline ] { url = "/privacy", label = text "Privacy Policy" } ] ]
                            ]
                        , column [ alignBottom, alignRight ]
                            [ row [ spacingXY 60 0, centerX, alignRight ]
                                [ row []
                                    [ newTabLink
                                        []
                                        { url = "https://www.ycombinator.com/companies/flint"
                                        , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/YC_logo.svg?new", description = "Flint" }
                                        }
                                    ]
                                , row []
                                    [ newTabLink
                                        []
                                        { url = "https://github.com/withflint"
                                        , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/github_logo.svg?new", description = "Flint" }
                                        }
                                    ]
                                , row []
                                    [ newTabLink
                                        []
                                        { url = "https://www.linkedin.com/company/withflint/"
                                        , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/linkedin-icon-2.svg?new", description = "Flint" }
                                        }
                                    ]
                                ]
                            ]
                        ]
                    , row [ alignBottom, alignRight ]
                        [ image [ width (px 165), height (px 100) ]
                            { src = "/static/images/blob-2.svg"

                            -- default dimension 214x149
                            , description = ""
                            }
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
                            , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/YC_logo.svg?new", description = "Flint" }
                            }
                        ]
                    , row []
                        [ newTabLink
                            []
                            { url = "https://github.com/withflint"
                            , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/github_logo.svg?new", description = "Flint" }
                            }
                        ]
                    , row []
                        [ newTabLink
                            []
                            { url = "https://www.linkedin.com/company/withflint/"
                            , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/linkedin-icon-2.svg?new", description = "Flint" }
                            }
                        ]
                    ]
                ]
            ]
        , row
            [ width fill, paddingEach { top = 30, bottom = 10, left = 0, right = 0 } ]
            [ image [ width (px 50), height (px 30), centerX ]
                { src = "/static/images/logo.svg?new"
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
    , ( NurseCareers "", "Nurse Careers" )
    , ( Contact, "Contact" )
    , ( JoinTheTeam "", "Join the Team" )
    , ( Blog "", "Blog" )
    ]
