module Faq.View exposing (..)

import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , alpha
        , centerX
        , centerY
        , column
        , fill
        , height
        , link
        , maximum
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , spacingXY
        , text
        , width
        )
import Element.Background as Background exposing (gradient)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Faq.Types exposing (Model, Msg(..))
import Layout exposing (Layout, footer, menu)
import Router.Routes exposing (Page(..), toPath)
import String exposing (lines)
import Styles exposing (buttons, colors, heading)


view : Model -> Layout Msg
view model =
    { phone =
        [ phoneHeader model.title
        , column
            [ centerX
            , width fill
            , height fill
            , paddingXY 20 40
            ]
            (desktopLayout model
                ++ footer.desktop
            )
        ]
    , tablet =
        [ desktopHeader model.title
        , column
            [ centerX
            , width <| maximum 1500 fill
            , height fill
            , paddingXY 100 40
            ]
            (desktopLayout model
                ++ footer.desktop
            )
        ]
    , desktop =
        [ desktopHeader model.title
        , column
            [ centerX
            , width <| maximum 1500 fill
            , height fill
            , paddingXY 100 40
            ]
            (desktopLayout model
                ++ footer.desktop
            )
        ]
    }


desktopHeader : String -> Element Msg
desktopHeader headerTitle =
    column [ width fill, Background.color colors.blue1, paddingXY 100 0 ]
        [ row [ width <| maximum 1300 fill, paddingXY 0 40, centerX ]
            [ column [ width fill ]
                [ Element.link []
                    { url = toPath Home
                    , label =
                        Element.image [ centerY, alignLeft, width (px 100), height (px 50) ]
                            { src = "/static/images/logo-white.svg"
                            , description = "Flint"
                            }
                    }
                ]
            , column [ width fill, alignRight ]
                [ column (width fill :: Styles.paragraph)
                    [ row [ spacingXY 30 0, alignRight ] <|
                        List.map
                            (\( path, label ) ->
                                row []
                                    [ link [ padding 5, Font.color colors.white3 ]
                                        { url = toPath path
                                        , label = text label
                                        }
                                    ]
                            )
                            menu
                    ]
                ]
            ]
        , row [ width <| maximum 1300 fill, paddingXY 0 0, centerX ]
            [ column [ width fill ]
                [ paragraph
                    [ centerX
                    , centerY
                    , Font.center
                    , Font.color colors.white3
                    , Styles.headFont
                    , Font.size 40
                    , paddingEach { top = 0, bottom = 40, left = 0, right = 0 }
                    ]
                    [ text headerTitle
                    ]
                ]
            ]
        ]


phoneHeader : String -> Element Msg
phoneHeader headerTitle =
    column [ width fill, Background.color colors.blue1, paddingXY 30 0 ]
        [ row [ width <| maximum 1500 fill, paddingXY 0 40, centerX ]
            [ Element.link []
                { url = toPath Home
                , label =
                    Element.image [ centerY, alignLeft, width (px 100), height (px 50) ]
                        { src = "/static/images/logo-white.svg"
                        , description = "Flint"
                        }
                }
            ]
        , row [ width fill, centerX ]
            [ column [ width fill ]
                [ paragraph
                    [ centerX
                    , centerY
                    , Font.center
                    , Font.color colors.white3
                    , Styles.headFont
                    , Font.size 40
                    , paddingEach { top = 0, bottom = 40, left = 0, right = 0 }
                    ]
                    [ text headerTitle
                    ]
                ]
            ]
        ]


desktopLayout : Model -> List (Element Msg)
desktopLayout model =
    [ column [ width fill ]
        (model.faqs
            |> List.indexedMap
                (\index ( title, content ) ->
                    column
                        [ width fill
                        , paddingXY 20 0
                        ]
                        [ row
                            [ width fill
                            , centerY
                            ]
                            [ row
                                [ width fill
                                , Font.size 30
                                , Font.color colors.deepBlue1
                                , paddingEach { top = 40, right = 10, bottom = 40, left = 0 }
                                ]
                                [ paragraph [] [ text title ] ]
                            , if model.selectedTopic == index then
                                Input.button
                                    [ width (px 50)
                                    , height (px 50)
                                    , Background.color colors.gray3
                                    , Border.rounded 25
                                    , alpha 0.5
                                    ]
                                    { label =
                                        Element.image []
                                            { src = "/static/images/icons8-cross-48.png"
                                            , description = "Flint"
                                            }
                                    , onPress = Just Hide
                                    }

                              else
                                Input.button
                                    [ width (px 50)
                                    , height (px 50)
                                    , Background.color colors.gray3
                                    , Border.rounded 25
                                    , alpha 0.5
                                    ]
                                    { label =
                                        Element.image []
                                            { src = "/static/images/icons8-plus-48.png"
                                            , description = "Flint"
                                            }
                                    , onPress = Just (Select index)
                                    }
                            ]
                        , if model.selectedTopic == index then
                            renderAnswer content

                          else
                            Element.none
                        , row
                            [ width fill
                            , height (px 1)
                            , Background.color colors.gray1
                            ]
                            []
                        ]
                )
        )
    ]


renderAnswer : String -> Element msg
renderAnswer lst =
    column
        [ paddingEach { top = 0, right = 0, bottom = 40, left = 0 }
        , Font.alignLeft
        ]
        (List.map
            (\l ->
                paragraph [ Font.size 17, paddingXY 0 4 ] [ text l ]
            )
            (lines lst)
        )


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
