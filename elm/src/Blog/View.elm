module Blog.View exposing (view)

import Blog.Types exposing (Article, ArticleState(..), Model, Msg)
import Device
import Element
    exposing
        ( Element
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , html
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
        , spacingXY
        , text
        , textColumn
        , width
        )
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes as HtmlAttr
import Layout exposing (Layout, footer)
import Mark
import Router.Routes as R exposing (toPath)
import Styles exposing (colors, css, hf, palette, wf)


view : Device.Device -> Model -> Layout Msg
view device model =
    let
        bg =
            [ css "background" "#DAE9FF"
            , css "background" "linear-gradient(180deg, #FFFBF8 0%, #DAE9FF 102.99%)"
            ]
    in
    { phone =
        [ blogPhoneHeader device
        , column
            ([ wf, hf ] ++ bg)
            [ column
                [ centerX
                , width <| maximum 1500 fill
                , height fill
                , paddingXY 20 40
                ]
                (blogPhoneView model.article)
            ]
        , column [ wf ] footer.phone
        ]
    , tablet =
        [ row [ wf ] <|
            blogHeader device
        , column ([ wf, hf ] ++ bg)
            [ column
                [ centerX
                , width <| maximum 1500 fill
                , height fill
                , paddingXY 40 40
                ]
                (blogView model.article)
            ]
        , column [ wf ] footer.phone
        ]
    , desktop =
        [ row [ wf ] <|
            blogHeader device
        , column ([ wf, hf ] ++ bg)
            [ column
                [ centerX
                , width <| maximum 1500 fill
                , height fill
                , paddingXY 100 40
                ]
                (blogView model.article)
            ]
        , column [ wf ] footer.desktop
        ]
    }


blogPhoneHeader : Device.Device -> Element msg
blogPhoneHeader device =
    let
        bg =
            [ css "background" "rgb(68,55,109)"
            , css "background" "linear-gradient(275.4deg, #E54848 -14.67%, #7B3D61 14.83%, #51497F 55.96%, #616297 92.44%, #A7C8F9 127.36%)"
            ]

        logo =
            row
                [ css "position" "absolute"
                , css "left" "44px"
                , css "top" "20px"
                , css "z-index" "100"
                ]
                [ Element.link
                    []
                    { url = toPath R.Home
                    , label =
                        Element.image
                            [ width (px 110), height (px 54) ]
                            { src = "/static/images/logo.svg?new", description = "Flint" }
                    }
                ]

        blob =
            row [ css "position" "relative" ]
                [ row
                    [ alignTop
                    , css "position" "relative"
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
    in
    row ([ wf, height (px 140), css "position" "relative" ] ++ bg)
        [ column [ css "position" "absolute", css "top" "0", css "left" "0" ]
            [ row [ css "width" "80%", css "height" "80%" ] [ blob ]
            ]
        , column [ wf ] []
        ]


blogHeader : Device.Device -> List (Element msg)
blogHeader device =
    let
        bg =
            [ css "background" "#DAE9FF"
            , css "background" "linear-gradient(275.4deg, #E54848 -14.67%, #7B3D61 14.83%, #51497F 55.96%, #616297 92.44%, #A7C8F9 127.36%)"
            ]

        logo =
            Element.image [ centerX, width (px 100), height (px 50) ] { src = "/static/images/logo-white.svg?new", description = "Flint" }
    in
    [ row ([ wf, height (px 136) ] ++ bg)
        [ -- GAP
          row [ width <| fillPortion 2 ] []

        -- LOGO
        , row [ width <| fillPortion 1 ]
            [ Element.link
                []
                { url = toPath R.Home
                , label =
                    el
                        [ Font.center
                        , Font.color palette.white
                        , mouseOver [ Font.color colors.carminePink ]
                        ]
                        logo
                }
            ]

        -- GAP
        , row [ width <| fillPortion 8 ] []

        -- MENU
        , row [ width <| fillPortion 2, spacingXY 24 0 ]
            [ Element.link
                []
                { url = toPath R.Partnerships
                , label =
                    el
                        Styles.menu
                        (text "Partnerships")
                }
            , Element.link
                []
                { url = toPath <| R.NurseCareers ""
                , label =
                    el Styles.menu (text "Nurse Careers")
                }
            ]

        -- GAP
        , row [ width <| fillPortion 2 ] []
        ]
    ]


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
                [ paragraph [ Font.size 40, Font.bold, Font.color palette.primary, mouseOver [ Font.color colors.carminePink ], Styles.headFont ] [ text article.title ]
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
                [ paragraph [ Font.bold, Font.size 46, Styles.headFont ] [ text article.title ]
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
        [ paragraph [ Font.size 24, Font.bold, width fill, Styles.headFont ] [ go <| text article.title ]
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
        , go <|
            row
                [ Font.size 14

                -- C this color might need change
                , Font.color colors.gray1
                , Font.underline
                , paddingEach { top = 0, right = 0, left = 0, bottom = 10 }
                ]
                [ text "Read more" ]
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
                [ paragraph [ Font.bold, Font.size 32, Styles.headFont ] [ text article.title ]
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
