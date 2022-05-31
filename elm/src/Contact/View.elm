module Contact.View exposing (view)

import Contact.Types exposing (Model)
import Element
    exposing
        ( Element
        , centerX
        , column
        , el
        , fill
        , height
        , link
        , maximum
        , minimum
        , mouseOver
        , padding
        , paddingEach
        , paddingXY
        , paragraph
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
import Layout exposing (Layout, footer, header)
import Styles exposing (buttons, colors, heading)


view : Model -> Layout msg
view _ =
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
                ++ desktopLayout
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
                    [ text "Contact Us" ]
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
                            ++ [ width <| maximum 240 fill
                               , Font.color colors.white3
                               , centerX
                               , Font.center
                               , mouseOver [ Background.color colors.blue1 ]
                               ]
                        )
                        { url = "mailto:healthcare@withflint.com", label = text "healthcare@withflint.com" }
                    , link
                        (Styles.paragraph
                            ++ buttons.secondary
                            ++ [ width <| maximum 240 fill
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
                            ++ [ width <| maximum 240 fill
                               , Font.color colors.white3
                               , centerX
                               , Font.center
                               , mouseOver [ Background.color colors.blue1 ]
                               ]
                        )
                        { url = "mailto:success@withflint.com", label = text "success@withflint.com" }
                    , link
                        (Styles.paragraph
                            ++ buttons.secondary
                            ++ [ width <| maximum 240 fill
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
                    [ text "Contact Us" ]
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
                        { url = "mailto:healthcare@withflint.com", label = text "healthcare@withflint.com" }
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
                        { url = "mailto:success@withflint.com", label = text "success@withflint.com" }
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


desktopLayout : List (Element msg)
desktopLayout =
    [ column
        [ width fill
        , height fill
        , centerX
        ]
        [ row [ width fill, paddingXY 0 100, spacing 10 ]
            [ column [ width fill, width (minimum 600 shrink) ]
                [ paragraph
                    heading
                    [ text "Contact Us" ]
                ]
            ]
        , row [ width fill, spacingXY 20 20, paddingEach { top = 5, bottom = 120, left = 0, right = 0 } ]
            [ column
                [ width fill
                , centerX
                , height (maximum 500 shrink)
                , Border.color colors.white2
                , Border.rounded 3
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 2
                    , blur = 8
                    , color = colors.gray3
                    }
                , padding 30
                , spacingXY 0 20
                ]
                [ row
                    (heading
                        ++ [ centerX
                           , Font.size 30
                           , Font.color colors.blue1
                           ]
                    )
                    [ text copy.staffingExpertsHead ]
                , row [ paddingEach { top = 50, bottom = 0, left = 0, right = 0 }, centerX ]
                    [ paragraph
                        (Styles.paragraph
                            ++ [ Font.center
                               , height <| minimum 200 fill
                               ]
                        )
                        [ text copy.staffingExperts ]
                    ]
                , row
                    (Styles.paragraph
                        ++ [ centerX
                           , width <| maximum 300 fill
                           , Font.size 20
                           , Font.color colors.gray2
                           , Border.rounded 3
                           ]
                    )
                    [ el [ centerX ] <| text "healthcare@withflint.com" ]
                , row
                    (Styles.paragraph
                        ++ [ centerX
                           , width <| maximum 300 fill
                           , Font.size 20
                           , Font.color colors.gray2
                           , Border.rounded 3
                           ]
                    )
                    [ el [ centerX ] <| text "+1 (844) 677-1180" ]
                ]
            , column
                [ width fill
                , height (maximum 500 shrink)
                , centerX
                , Border.color colors.white2
                , Border.rounded 3
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 2
                    , blur = 8
                    , color = colors.gray3
                    }
                , padding 30
                , spacingXY 0 20
                ]
                [ row
                    (heading
                        ++ [ centerX
                           , Font.size 30
                           , Font.color colors.blue1
                           ]
                    )
                    [ text copy.talentExpertsHead ]
                , row [ paddingEach { top = 50, bottom = 0, left = 0, right = 0 } ]
                    [ paragraph
                        (Styles.paragraph
                            ++ [ Font.center
                               , height <| minimum 200 fill
                               ]
                        )
                        [ text copy.talentExperts ]
                    ]
                , row
                    (Styles.paragraph
                        ++ [ centerX
                           , width <| maximum 300 fill
                           , Font.size 20
                           , Font.color colors.gray2
                           ]
                    )
                    [ el [ centerX ] <| text "success@withflint.com" ]
                , row
                    (Styles.paragraph
                        ++ [ centerX
                           , width <| maximum 300 fill
                           , Font.size 20
                           , Border.rounded 3
                           , Font.color colors.gray2
                           ]
                    )
                    [ el [ centerX ] <| text "+1 (844) 677-1180" ]
                ]
            ]
        ]
    ]


copy :
    { staffingExpertsHead : String
    , staffingExperts : String
    , talentExpertsHead : String
    , talentExperts : String
    }
copy =
    { staffingExpertsHead = "Healthcare Partnerships"
    , staffingExperts = "To learn more about what an optimized staffing solution looks like, meet with our partnership team."
    , talentExpertsHead = "Nurse Talent Team"
    , talentExperts = "Are you an internationally educated nurse? We can help you relocate to the United States of America; we offer all-inclusive packages, including immigration, green cards, transport, accommodations and the best job offers. Text, call or email us!"
    }
