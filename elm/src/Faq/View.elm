module Faq.View exposing (..)

import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , fill
        , height
        , link
        , maximum
        , minimum
        , mouseOver
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , shrink
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Faq.Types exposing (Model, Msg(..))
import Layout exposing (Layout, footer, header)
import Router.Routes exposing (Page(..))
import Styles exposing (buttons, colors, heading)


view : Model -> Layout Msg
view model =
    { phone =
        [ column
            [ centerX
            , width <| maximum 1500 fill
            , height fill
            , paddingXY 20 40
            ]
            (header.phone
                ++ phoneLayout
                ++ footer.phone
            )
        ]
    , tablet =
        [ column
            [ centerX
            , width <| maximum 1500 fill
            , height fill
            , paddingXY 100 40
            ]
            (header.tablet
                ++ tabletLayout
                ++ footer.tablet
            )
        ]
    , desktop =
        [ column
            [ centerX
            , width <| maximum 1500 fill
            , height fill
            , paddingXY 100 40
            ]
            (header.desktop
                ++ desktopLayout model
                ++ footer.desktop
            )
        ]
    }


phoneLayout : List (Element msg)
phoneLayout =
    [ column
        [ width fill
        , paddingXY 30 0
        , spacingXY 0 20
        ]
        [ row [ width fill, height fill, paddingXY 0 50 ]
            [ column [ centerX, width fill ]
                [ row
                    (heading ++ [ centerX ])
                    [ text "FAQ for Internationally Educated Nurses" ]
                ]
            ]
        , column [ width fill, height fill, spacingXY 30 20 ]
            [ row
                [ width fill
                , height fill
                , paddingXY 10 10
                , spacingXY 20 20
                , Border.color colors.white2
                , Border.rounded 3
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 2
                    , blur = 8
                    , color = colors.gray3
                    }
                , height <| minimum 500 fill
                , width <| maximum 700 fill
                , centerX
                ]
                [ column [ width fill, spacingXY 0 15, paddingXY 0 20 ]
                    [ row (heading ++ [ centerX, Font.size 30, Font.color colors.blue1 ]) [ text copy.staffingExpertsHead ]
                    , row [ centerX ]
                        [ paragraph
                            (Styles.paragraph
                                ++ [ Font.center
                                   , paddingXY 40 30
                                   , height <| minimum 120 fill
                                   ]
                            )
                            [ text copy.staffingExperts ]
                        ]
                    , link
                        (Styles.paragraph
                            ++ buttons.primary
                            ++ [ width <| maximum 200 fill
                               , Font.color colors.white3
                               , centerX
                               , Font.center
                               , mouseOver [ Background.color colors.blue1 ]
                               ]
                        )
                        { url = "mailto:staffing@withflint.com", label = text "staffing@withflint.com" }
                    , link
                        (Styles.paragraph
                            ++ buttons.secondary
                            ++ [ width <| maximum 200 fill
                               , Font.color colors.white3
                               , centerX
                               , Font.center
                               , mouseOver [ Background.color colors.gray1 ]
                               ]
                        )
                        { url = "tel:+1 (844) 677-1180", label = text "+1 (844) 677-1180" }
                    ]
                ]
            , row
                [ width fill
                , height fill
                , paddingXY 10 10
                , spacingXY 20 20
                , Border.color colors.white2
                , Border.rounded 3
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 2
                    , blur = 8
                    , color = colors.gray3
                    }
                , height <| minimum 600 fill
                , width <| maximum 700 fill
                , centerX
                ]
                [ column [ width fill, spacingXY 0 15, paddingXY 0 20 ]
                    [ row (heading ++ [ centerX, Font.size 30, Font.color colors.blue1 ]) [ text copy.talentExpertsHead ]
                    , row [ centerX ]
                        [ paragraph
                            (Styles.paragraph
                                ++ [ Font.center
                                   , paddingXY 40 30
                                   , height <| minimum 120 fill
                                   ]
                            )
                            [ text copy.talentExperts ]
                        ]
                    , link
                        (Styles.paragraph
                            ++ buttons.primary
                            ++ [ width <| maximum 200 fill
                               , Font.color colors.white3
                               , centerX
                               , Font.center
                               , mouseOver [ Background.color colors.blue1 ]
                               ]
                        )
                        { url = "mailto:talent@withflint.com", label = text "talent@withflint.com" }
                    , link
                        (Styles.paragraph
                            ++ buttons.secondary
                            ++ [ width <| maximum 200 fill
                               , Font.color colors.white3
                               , centerX
                               , Font.center
                               , mouseOver [ Background.color colors.gray1 ]
                               ]
                        )
                        { url = "tel:+1 (844) 677-1180", label = text "+1 (844) 677-1180" }
                    ]
                ]
            ]
        ]
    ]


tabletLayout : List (Element msg)
tabletLayout =
    [ column
        [ width fill
        , paddingXY 50 0
        ]
        [ row [ width fill, height fill, paddingXY 0 50 ]
            [ column [ centerX, width fill, width (minimum 600 shrink) ]
                [ row
                    (heading ++ [ centerX ])
                    [ text "FAQ" ]
                ]
            ]
        , column [ width fill, height fill, spacingXY 30 20 ]
            [ row
                [ width fill
                , height fill
                , paddingXY 10 10
                , spacingXY 20 20
                , Border.color colors.white2
                , Border.rounded 3
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 2
                    , blur = 8
                    , color = colors.gray3
                    }
                , height <| minimum 500 fill
                , width <| maximum 700 fill
                , centerX
                ]
                [ column
                    [ width fill
                    , spacingXY 0 25
                    , paddingXY 0 40
                    ]
                    [ row
                        (heading
                            ++ [ centerX
                               , Font.size 30
                               , height <| minimum 50 fill
                               , Font.color colors.blue1
                               ]
                        )
                        [ text copy.staffingExpertsHead ]
                    , row [ width <| maximum 500 fill, centerX ]
                        [ paragraph
                            (Styles.paragraph
                                ++ [ Font.center
                                   , paddingXY 40 0
                                   , height <| minimum 120 fill
                                   ]
                            )
                            [ text copy.staffingExperts ]
                        ]
                    , link
                        (Styles.paragraph
                            ++ buttons.primary
                            ++ [ width <| maximum 300 fill
                               , Font.color colors.white3
                               , Font.center
                               , centerX
                               , mouseOver [ Background.color colors.blue1 ]
                               ]
                        )
                        { url = "mailto:staffing@withflint.com", label = text "staffing@withflint.com" }
                    , link
                        (Styles.paragraph
                            ++ buttons.secondary
                            ++ [ width <| maximum 300 fill
                               , Font.color colors.white3
                               , Font.center
                               , centerX
                               , mouseOver [ Background.color colors.gray1 ]
                               ]
                        )
                        { url = "tel:+1 (844) 677-1180", label = text "+1 (844) 677-1180" }
                    ]
                ]
            , row
                [ width fill
                , height fill
                , paddingXY 10 10
                , spacingXY 20 20
                , Border.color colors.white2
                , Border.rounded 3
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 2
                    , blur = 8
                    , color = colors.gray3
                    }
                , height <| minimum 500 fill
                , width <| maximum 700 fill
                , centerX
                ]
                [ column
                    [ width fill
                    , spacingXY 0 25
                    , paddingXY 0 40
                    ]
                    [ row
                        (heading
                            ++ [ centerX
                               , Font.size 30
                               , height <| minimum 80 fill
                               , Font.color colors.blue1
                               ]
                        )
                        [ text copy.talentExpertsHead ]
                    , row [ width <| maximum 500 fill, centerX ]
                        [ paragraph
                            (Styles.paragraph
                                ++ [ Font.center
                                   , paddingEach { left = 40, right = 40, top = 0, bottom = 30 }
                                   , height <| minimum 120 fill
                                   ]
                            )
                            [ text copy.talentExperts ]
                        ]
                    , link
                        (Styles.paragraph
                            ++ buttons.primary
                            ++ [ width <| maximum 300 fill
                               , Font.color colors.white3
                               , centerX
                               , Font.center
                               , mouseOver [ Background.color colors.blue1 ]
                               ]
                        )
                        { url = "mailto:talent@withflint.com", label = text "talent@withflint.com" }
                    , link
                        (Styles.paragraph
                            ++ buttons.secondary
                            ++ [ width <| maximum 300 fill
                               , Font.color colors.white3
                               , centerX
                               , Font.center
                               , mouseOver [ Background.color colors.gray1 ]
                               ]
                        )
                        { url = "tel:+1 (844) 677-1180", label = text "+1 (844) 677-1180" }
                    ]
                ]
            ]
        ]
    ]


desktopLayout : Model -> List (Element Msg)
desktopLayout model =
    [ column
        [ width fill
        , height fill
        , centerX
        ]
        [ row [ width fill, paddingXY 0 100, spacing 10 ]
            [ column [ width fill ]
                [ paragraph
                    heading
                    [ text "FAQ for Internationally Educated Nurses" ]
                ]
            ]
        , column [ width fill ]
            (contents
                |> List.indexedMap
                    (\index ( title, content ) ->
                        column [ width fill, spacingXY 20 20, paddingEach { top = 5, bottom = 120, left = 20, right = 20 } ]
                            [ column
                                [ width fill
                                , height (px 1)
                                , Background.color colors.gray1
                                ]
                                []
                            , row
                                [ width fill
                                , centerY
                                , paddingEach { top = 5, bottom = 0, left = 20, right = 20 }
                                ]
                                [ row
                                    [ width fill
                                    , Font.size 30
                                    ]
                                    [ text title ]
                                , if model.selectedTopic == index then
                                    Input.button (centerY :: Font.size 15 :: Styles.buttons.primary)
                                        { label =
                                            Element.image []
                                                { src = "/static/images/github_logo.svg"
                                                , description = "Flint"
                                                }
                                        , onPress = Just Hide
                                        }

                                  else
                                    Input.button (centerY :: Font.size 15 :: Styles.buttons.primary)
                                        { label =
                                            Element.image []
                                                { src = "/static/images/linkedin_logo.svg"
                                                , description = "Flint"
                                                }
                                        , onPress = Just (Select index)
                                        }
                                ]
                            , if model.selectedTopic == index then
                                paragraph
                                    (Styles.paragraph
                                        ++ [ Font.alignLeft
                                           , Font.size 20
                                           , paddingEach { top = 30, bottom = 20, left = 20, right = 20 }
                                           , height <| minimum 120 fill
                                           ]
                                    )
                                    [ text content ]

                              else
                                Element.none
                            ]
                    )
            )
        ]
    ]


contents : List ( String, String )
contents =
    [ ( "Who is this for?", "International educated Registered Nurses (RN) with a willingness to relocate to the US permanently to work as a RN." )
    , ( "How does it work?", "We work with US employers such as hospitals to source great nurses internationally. We work with these international nurses getting them licensed, arranging a new job and immigration to ultimately begin their new life in the US." )
    , ( "How do I apply?", "To get started you can visit withflint.com and click the apply button." )
    ]


copy :
    { staffingExpertsHead : String
    , staffingExperts : String
    , talentExpertsHead : String
    , talentExperts : String
    }
copy =
    { staffingExpertsHead = "Staffing Experts"
    , staffingExperts = "To learn more about what an optimized staffing solution looks like, meet with our staffing experts."
    , talentExpertsHead = "Talent Experts"
    , talentExperts = "Are you an internationally educated health care worker? We can help you relocate to the United States of America; we offer all-inclusive packages, including immigration, green cards, transport, accommodations and A+ job offers. Text, call or email us!"
    }
