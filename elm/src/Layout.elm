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
        , mouseOver
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
import Styles exposing (colors, css, hf, palette, pl, pt, wf)


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
        link label page =
            Element.link
                []
                { url = toPath page
                , label =
                    Element.paragraph [ Font.center ] [ text label ]
                }

        linkFooter label page =
            Element.link
                [ wf
                ]
                { url = toPath page
                , label =
                    Element.paragraph [ Font.center ] [ text label ]
                }

        privacy =
            row [ Font.size 10 ]
                [ newTabLink [ underline ] { url = "/privacy", label = text "Privacy Policy" }
                ]

        default : List (Element msg)
        default =
            [ row
                [ width fill
                , height fill
                , css "position" "relative"
                , Font.size 12
                ]
                [ row [ css "position" "absolute", css "top" "0", css "left" "-12px" ]
                    [ image [ width (px 164), height (px 114) ]
                        { src = "/static/images/footer-blob-left.svg"
                        , description = ""
                        }
                    ]
                , wrappedRow [ wf, hf, paddingXY 0 48 ]
                    [ row [ width (px 200) ] []
                    , row [ wf, spaceEvenly ]
                        [ -- FLINT LOGO
                          row [ hf, centerY, width <| fillPortion 3 ]
                            [ Element.link
                                []
                                { url = toPath Home
                                , label =
                                    image [ width (px 80), height (px 50) ]
                                        { src = "/static/images/logo.svg?new"
                                        , description = "Flint"
                                        }
                                }
                            ]

                        -- MENU
                        , column [ hf, width <| fillPortion 3, spacingXY 0 12 ]
                            [ link "Blog" (Blog "")

                            -- , link "About Us" AboutUs
                            , link "Join the Team" (JoinTheTeam "")
                            ]
                        , column [ hf, width <| fillPortion 3, Font.color palette.primary, spacingXY 0 12 ]
                            [ Element.paragraph [] [ text "Healthcare Partnerships" ]
                            , column [ spacingXY 0 6 ]
                                [ Element.paragraph [] [ text "healthcare@withflint.com" ]
                                , Element.paragraph [] [ text "+1 (844) 677-1180" ]
                                ]
                            ]
                        , column [ hf, width <| fillPortion 3, Font.color palette.primary, spacingXY 0 12 ]
                            [ Element.paragraph [] [ text "Nurse Success" ]
                            , column [ spacingXY 0 6 ]
                                [ Element.paragraph [] [ text "success@withflint.com" ]
                                , Element.paragraph [] [ text "+1 (844) 677-1180" ]
                                ]
                            ]
                        ]
                    , row [ width (px 200) ] []
                    ]
                , row [ css "position" "absolute", css "right" "2px", css "bottom" "0px" ]
                    [ image [ width (px 165), height (px 100), alignRight ]
                        { src = "/static/images/blob-2.svg"

                        -- default dimension 214x149
                        , description = ""
                        }
                    ]
                ]

            -- privacy and social sites
            , row [ wf, Background.color palette.cremeLighter, paddingXY 88 24, centerX ]
                [ row [ wf, spaceEvenly ]
                    [ row [ spaceEvenly ] [ privacy ]
                    , row [ spaceEvenly, wf ] [ paragraph [ Font.center, Font.size 10 ] [ text "© 2022 Flint, all rights reserved" ] ]
                    , row [ spaceEvenly, spacingXY 36 0 ]
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
            ]
    in
    { phone =
        [ column [ wf, hf, paddingXY 0 48, Background.color palette.cremeLight, spacingXY 0 32 ]
            [ column [ centerX ]
                [ Element.image [ width (px 90), height (px 34) ] { src = "/static/images/logo.svg?new", description = "Flint" }
                ]
            , column [ Font.center, centerX, spacingXY 0 24, Font.color palette.primary ]
                [ row [ spacingXY 12 0, centerX ]
                    [ linkFooter "Nurse Success" (NurseCareers "")
                    , linkFooter "Partnerships" Partnerships
                    ]
                , row [ spacingXY 16 0, Font.color palette.primary ]
                    [ linkFooter "Blog" (Blog "")

                    -- , linkFooter "About Us" AboutUs
                    , linkFooter "Join the Team" (JoinTheTeam "")
                    ]
                ]
            , column [ Font.center, centerX, spacingXY 0 16 ]
                [ column [ hf, spacingXY 0 12 ]
                    [ Element.paragraph [] [ text "Healthcare Partnerships" ]
                    , column [ spacingXY 0 6 ]
                        [ Element.paragraph [] [ text "healthcare@withflint.com" ]
                        , Element.paragraph [] [ text "+1 (844) 677-1180" ]
                        ]
                    ]
                , column [ hf, spacingXY 0 12 ]
                    [ Element.paragraph [] [ text "Nurse Success" ]
                    , column [ spacingXY 0 6 ]
                        [ Element.paragraph [] [ text "success@withflint.com" ]
                        , Element.paragraph [] [ text "+1 (844) 677-1180" ]
                        ]
                    ]
                ]
            ]

        -- social and privacy
        , column [ wf, paddingXY 12 36, Background.color palette.cremeLighter, spacingXY 0 32 ]
            [ row [ centerX, spacingXY 16 0 ]
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
            , row [ spaceEvenly, wf ] [ paragraph [ Font.center, Font.size 10 ] [ text "© 2022 Flint, all rights reserved" ] ]
            , row [ centerX ] [ privacy ]
            ]
        ]
    , tablet = default
    , desktop = default
    }


menu : List ( Page, String )
menu =
    [ ( Home, "Home" )
    , ( NurseCareers "", "Nurse Success" )
    , ( Blog "", "Blog" )
    , ( AboutUs, "About Us" )
    , ( Contact, "Contact" )
    , ( JoinTheTeam "", "Join the Team" )
    ]
