module Blog.View exposing (view)

import Blog.Types exposing (Article, ArticleState(..), Model, Msg)
import Element
    exposing
        ( Element
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , image
        , link
        , maximum
        , mouseOver
        , newTabLink
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , spacing
        , text
        , textColumn
        , width
        )
import Element.Border as Border
import Element.Font as Font
import Layout exposing (Layout, footer, header)
import Mark
import Router.Routes as R
import Styles exposing (colors)


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
                ++ blogPhoneView model.article
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
                ++ blogView model.article
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
                ++ blogView model.article
                ++ footer.desktop
            )
        ]
    }


blogView : ArticleState -> List (Element Msg)
blogView article =
    case article of
        Loading ->
            [ Mark.loading
            ]

        NotFound ->
            [ Mark.oops
            ]

        List articles ->
            [ articles
                |> List.indexedMap (hr summaryView)
                |> List.map (row [ width fill, centerX, alignTop ])
                |> column [ width fill, centerX, alignTop, width <| maximum 850 fill, paddingXY 80 40, spacing 80 ]
            ]

        Loaded article_ ->
            article_ |> articleView


hr : (c -> List (Element msg)) -> number -> c -> List (Element msg)
hr f index article =
    [ column [ width fill ]
        ((if index == 0 then
            []

          else
            [ el
                [ width fill
                , centerX
                , Border.widthEach
                    { bottom = 0
                    , left = 0
                    , right = 0
                    , top = 1
                    }
                , paddingEach
                    { bottom = 15
                    , left = 0
                    , right = 0
                    , top = 0
                    }
                , Border.color
                    colors.white1
                ]
                (text "")
            ]
         )
            ++ f article
        )
    ]


summaryView : Article -> List (Element msg)
summaryView article =
    [ link [ width fill, centerX ]
        { url = R.toPath <| R.Blog article.slug
        , label =
            column
                [ spacing 30, centerX ]
                [ paragraph [ Styles.headFont, Font.size 40, Font.bold, mouseOver [ Font.color colors.blue1 ] ] [ text article.title ]
                , row
                    [ spacing 5
                    , alignTop
                    , width (fill |> maximum 100)
                    , Font.size 14
                    ]
                    [ image [ Border.rounded 25, centerY, width (px 28), height (px 28) ] { src = article.avatar, description = article.author }
                    , text article.author
                    , el [] (text "·")
                    , el [] (text article.bio)
                    , el [] (text "·")
                    , el [ Font.color colors.gray2 ] (text article.date)
                    ]
                , column
                    (Styles.paragraph
                        ++ [ centerX
                           , spacing 40
                           ]
                    )
                    [ image
                        [ width fill
                        , paddingEach
                            { bottom = 0
                            , left = 0
                            , right = 0
                            , top = 0
                            }
                        ]
                        { src = "/static/images/blog/" ++ article.slug ++ ".jpeg", description = article.meta.description }
                    , paragraph [ width fill ] [ text article.sub ]
                    ]
                ]
        }
    ]


articleView : Article -> List (Element msg)
articleView article =
    [ case Mark.view article.body of
        Ok rendered ->
            column
                [ padding 80, width <| maximum 850 fill, spacing 40, centerX ]
                [ paragraph [ Styles.headFont, Font.bold, Font.size 46 ] [ text article.title ]
                , row
                    [ spacing 5
                    , alignTop
                    , width (fill |> maximum 100)
                    , Font.size 14
                    ]
                    [ newTabLink
                        []
                        { url = article.link
                        , label = image [ Border.rounded 25, centerY, width (px 28), height (px 28) ] { src = article.avatar, description = article.author }
                        }
                    , newTabLink
                        [ Font.underline ]
                        { url = article.link
                        , label = text article.author
                        }
                    , el [] (text "·")
                    , el [] (text article.bio)
                    , el [] (text "·")
                    , el [ Font.color colors.gray2 ] (text article.date)
                    ]
                , column
                    (Styles.paragraph
                        ++ [ centerX
                           , Font.size 20
                           , spacing 20
                           ]
                    )
                    (image
                        [ width fill
                        , paddingEach
                            { bottom = 20
                            , left = 0
                            , right = 0
                            , top = 0
                            }
                        ]
                        { src = "/static/images/blog/" ++ article.slug ++ ".jpeg", description = article.meta.description }
                        :: rendered
                    )
                ]

        Err _ ->
            Mark.oops
    ]


blogPhoneView : ArticleState -> List (Element Msg)
blogPhoneView article =
    case article of
        Loading ->
            [ Mark.loading
            ]

        NotFound ->
            [ Mark.oops
            ]

        List articles ->
            [ articles
                |> List.indexedMap (hr phoneSummaryView)
                |> List.map (row [ width fill, centerX, alignTop ])
                |> column
                    [ width fill
                    , centerX
                    , alignTop
                    , paddingXY 20 20
                    , spacing 10
                    ]
            ]

        Loaded article_ ->
            article_ |> articlePhoneView


phoneSummaryView : Article -> List (Element Msg)
phoneSummaryView article =
    let
        go : Element msg -> Element msg
        go ele =
            link [ width fill, mouseOver [ Font.color colors.blue1 ] ]
                { url = R.toPath <| R.Blog article.slug
                , label = ele
                }
    in
    [ column
        [ width fill
        , spacing 20
        , centerX
        ]
        [ paragraph [ Styles.headFont, Font.size 24, Font.bold, width fill ] [ go <| text article.title ]
        , go <|
            row
                [ spacing 5
                , alignTop
                , Font.size 14
                , width fill
                ]
                [ el [ Font.color colors.gray2 ] (text article.date)
                ]
        , go <|
            textColumn
                (Styles.paragraph
                    ++ [ width fill
                       , centerX
                       ]
                )
                [ image
                    [ width fill
                    ]
                    { src = "/static/images/blog/" ++ article.slug ++ ".jpeg", description = article.meta.description }
                , paragraph [ width fill ] [ text article.sub ]
                ]
        , go <| row [ Font.size 14, Font.color colors.gray1, Font.underline, paddingEach { top = 0, right = 0, left = 0, bottom = 10 } ] [ text "Read more" ]
        ]
    ]


articlePhoneView : Article -> List (Element msg)
articlePhoneView article =
    [ case Mark.view article.body of
        Ok rendered ->
            column
                [ paddingXY 20 20
                , width fill
                , spacing 30
                ]
                [ paragraph [ Styles.headFont, Font.bold, Font.size 32 ] [ text article.title ]
                , row
                    [ spacing 5
                    , alignTop
                    , width (fill |> maximum 100)
                    , Font.size 14
                    ]
                    [ newTabLink
                        []
                        { url = article.link
                        , label = image [ Border.rounded 25, centerY, width (px 28), height (px 28) ] { src = article.avatar, description = article.author }
                        }
                    , newTabLink
                        [ Font.underline ]
                        { url = article.link
                        , label = text article.author
                        }
                    , el [] (text "·")
                    , el [] (text article.bio)
                    ]
                , row [] [ el [ Font.color colors.gray2, Font.size 14 ] (text article.date) ]
                , column
                    (Styles.paragraph
                        ++ [ width fill
                           , centerX
                           , Font.size 18
                           , spacing 30
                           ]
                    )
                    (image
                        [ width fill
                        ]
                        { src = "/static/images/blog/" ++ article.slug ++ ".jpeg", description = article.meta.description }
                        :: rendered
                    )
                ]

        Err _ ->
            Mark.oops
    ]
