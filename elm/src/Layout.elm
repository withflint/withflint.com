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
        , fillPortion
        , height
        , image
        , link
        , newTabLink
        , padding
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
import Element.Font as Font exposing (underline)
import Html.Attributes exposing (wrap)
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, hf, palette, pl, wf)


type alias Layout msg =
    { phone : List (Element msg)
    , tablet : List (Element msg)
    , desktop : List (Element msg)
    }


layout : Device -> Layout msg -> Element msg
layout device views =
    case device of
        -- vp == viewport
        Phone vp ->
            column
                [ --     height fill
                  -- , width fill
                  -- , centerX
                  -- , alignTop
                  wf
                , hf
                , Background.color colors.cremeLight
                ]
                views.phone

        Tablet vp ->
            column
                [ -- height fill
                  -- , width fill
                  -- , centerX
                  -- , alignTop
                  wf
                , hf
                , Background.color colors.cremeDark
                ]
                views.tablet

        _ ->
            -- C changed
            column [ width fill, height fill ]
                [ column
                    [ --     height fill
                      -- , width fill
                      -- , centerX
                      -- , alignTop
                      wf
                    , hf
                    , Background.color colors.cremeDark
                    ]
                    views.desktop

                -- , column [ width fill, height fill ]
                --     footer.desktop
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
        link label url =
            Element.link
                []
                { url = url
                , label =
                    Element.paragraph [ Font.center ] [ text label ]
                }

        default : List (Element msg)
        default =
            [ row
                [ width fill
                , height fill

                -- , Background.color colors.blue1
                ]
                [ row [ alignTop, width <| fillPortion 2 ]
                    [ image [ width (px 164), height (px 114) ]
                        { src = "/static/images/footer-blob-left.svg"

                        -- default dimension 213x163
                        , description = ""
                        }
                    ]
                , row
                    [ width <| fillPortion 8, paddingXY 0 48, spaceEvenly ]
                    [ -- FLINT LOGO
                      column []
                        [ row
                            []
                            [ image [ width (px 80), height (px 50) ]
                                { src = "/static/images/logo.svg?new"
                                , description = "Flint"
                                }
                            ]
                        , row [ Font.size 10, paddingXY 0 10 ]
                            [ column [ spacing 20 ] [ text "© 2022 Flint, all rights reserved", newTabLink [ underline ] { url = "/privacy", label = text "Privacy Policy" } ] ]
                        ]

                    -- MENU
                    , row [ wf ]
                        [ row [ centerX, spacingXY 24 0 ]
                            [ link "Blog" "/blog/"
                            , link "Join the Team" "/join/"
                            , row [ wf, spacingXY 24 12 ]
                                [ column [ spacingXY 0 10 ]
                                    [ Element.paragraph [ Font.color palette.primary ] [ text "Healthcare Partnerships" ]
                                    , column [ spacingXY 0 6 ]
                                        [ Element.paragraph [] [ text "healthcare@withflint.com" ]
                                        , Element.paragraph [] [ text "+1 (844) 677-1180" ]
                                        ]
                                    ]
                                , column [ spacingXY 0 10 ]
                                    [ Element.paragraph [ Font.color palette.primary ] [ text "Nurse Success" ]
                                    , column [ spacingXY 0 6 ]
                                        [ Element.paragraph [] [ text "success@withflint.com" ]
                                        , Element.paragraph [] [ text "+1 (844) 677-1180" ]
                                        ]
                                    ]
                                ]
                            ]
                        ]

                    -- SOCIAL
                    , row [ spacingXY 60 0 ]
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
                , row [ alignBottom, width <| fillPortion 2 ]
                    [ image [ width (px 165), height (px 100), alignRight ]
                        { src = "/static/images/blob-2.svg"

                        -- default dimension 214x149
                        , description = ""
                        }
                    ]
                ]
            ]
    in
    { phone =
        [ column [ wf, paddingXY 0 48, alignBottom, Background.color colors.cremeDark ]
            [ row [ centerX ]
                [ row [ width <| fillPortion 2, alignLeft ]
                    [ Element.image [ width (px 80), height (px 24) ] { src = "/static/images/logo.svg?new", description = "Flint" }
                    ]
                , wrappedRow
                    [ width <| fillPortion 10, spacingXY 24 0 ]
                    [ link "Blog" "/blog/"
                    , link "Join the Team" "/join/"
                    ]
                ]
            , row [ centerX, Font.size 10, paddingXY 24 24 ]
                [ row [ spacing 20 ] [ text "© 2022 Flint, all rights reserved", newTabLink [ underline ] { url = "/privacy", label = text "Privacy Policy" } ] ]
            ]
        ]

    -- [ column
    --     [ Background.color palette.cremeLight
    --     , wf
    --     , Background.image "static/images/footer-blob-left.svg"
    --     ]
    --     [ row
    --         [ width fill
    --         , centerX
    --         , paddingXY 0 50
    --         ]
    --         [ -- FOOTER MENU
    --           column [ centerX, width fill, spacingXY 50 20, centerX, Font.size 15, Font.color palette.primary, Font.semiBold ]
    --             (menu |> List.map (\( path, label ) -> row [ width fill, centerX ] [ link [ centerX, padding 5 ] { url = toPath path, label = text label } ]))
    --         ]
    --     , row [ width fill, centerX ]
    --         [ column [ width fill, alignBottom ]
    --             [ row [ spacingXY 60 0, centerX ]
    --                 [ row []
    --                     [ newTabLink
    --                         []
    --                         { url = "https://www.ycombinator.com/companies/flint"
    --                         , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/YC_logo.svg?new", description = "Flint" }
    --                         }
    --                     ]
    --                 , row []
    --                     [ newTabLink
    --                         []
    --                         { url = "https://github.com/withflint"
    --                         , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/github_logo.svg?new", description = "Flint" }
    --                         }
    --                     ]
    --                 , row []
    --                     [ newTabLink
    --                         []
    --                         { url = "https://www.linkedin.com/company/withflint/"
    --                         , label = Element.image [ centerY, alignLeft, width (px 25), height (px 25) ] { src = "/static/images/linkedin-icon-2.svg?new", description = "Flint" }
    --                         }
    --                     ]
    --                 ]
    --             ]
    --         ]
    --     , row
    --         [ width fill, paddingEach { top = 30, bottom = 10, left = 0, right = 0 } ]
    --         [ image [ width (px 50), height (px 30), centerX ]
    --             { src = "/static/images/logo.svg?new"
    --             , description = "Flint"
    --             }
    --         ]
    --     , row [ centerX, width fill, Font.size 10, paddingXY 0 10 ]
    --         [ el [ centerX ] <| column [ spacing 20, centerX ] [ text "© 2022 Flint, all rights reserved", newTabLink [ underline, centerX ] { url = "/privacy", label = text "Privacy Policy" } ] ]
    --     ]
    -- ]
    , tablet = default
    , desktop = default
    }


menu : List ( Page, String )
menu =
    [ ( Home, "Home" )
    , ( NurseCareers "", "Nurse Careers" )
    , ( Blog "", "Blog" )
    , ( AboutUs, "About Us" )
    , ( Contact, "Contact" )
    , ( JoinTheTeam "", "Join the Team" )
    ]
