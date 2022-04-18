module FaqNurses.View exposing (view)

import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , link
        , maximum
        , minimum
        , none
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , rgb255
        , row
        , shrink
        , spacing
        , spacingXY
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.Input as Input
import Element.Lazy exposing (lazy2)
import FaqNurses.Types exposing (Faq, FormattedText(..), Model, Msg(..))
import Layout exposing (Layout, footer, menu)
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors)


view : Model -> Layout Msg
view model =
    { phone =
        [ phoneHeader
        , faqHeroTitlePhone model.heroTitle
        , column
            [ width fill
            , height fill
            , paddingXY 20 40
            , spacing 50
            , centerX
            ]
            (faqsView model :: footer.phone)
        ]
    , tablet =
        [ desktopHeader
        , faqHeroTitlePhone model.heroTitle
        , column
            [ width <| maximum 1500 fill
            , height fill
            , paddingXY 100 40
            , spacing 50
            , centerX
            ]
            (faqsView model :: footer.desktop)
        ]
    , desktop =
        [ desktopHeader
        , faqHeroTitlePhone model.heroTitle
        , column
            [ width <| maximum 1500 fill
            , height fill
            , paddingXY 100 40
            , spacing 50
            , centerX
            ]
            (faqsView model :: footer.desktop)
        ]
    }


faqsView : Model -> Element Msg
faqsView { faqs } =
    column [ width <| maximum 850 fill, height fill, Font.color colors.deepBlue1, spacingXY 0 40, centerX ]
        (List.indexedMap (lazy2 viewAFaq) faqs)


viewAFaq : Int -> Faq -> Element Msg
viewAFaq index faq =
    column
        [ spacingXY 0 0
        , Border.widthEach
            { bottom = 0
            , left = 0
            , right = 0
            , top =
                if index == 0 then
                    0

                else
                    1
            }
        , Border.color colors.white1
        , paddingEach
            { bottom = 0
            , left = 0
            , right = 0
            , top = 30
            }
        , width fill
        , height fill
        ]
        [ wrappedRow [ width fill, centerY ]
            [ lazy2 viewQuestion faq.id faq.question
            , if faq.isVisible then
                collapseOrExpandBtn "×" faq.id

              else
                collapseOrExpandBtn "+" faq.id
            ]
        , if faq.isVisible then
            column
                [ width <| maximum 750 fill
                , spacingXY 0 20
                , paddingEach
                    { bottom = 0
                    , left = 0
                    , right = 0
                    , top = 20
                    }
                ]
                (List.map viewAnswer faq.answer)

          else
            el [] none
        ]


viewQuestion : Int -> String -> Element Msg
viewQuestion id question =
    Input.button [ width fill ]
        { onPress = Just (ToggleVisibility id)
        , label = el [] (paragraph [ Styles.headFont, alignLeft, Font.medium, Styles.headFont ] [ text question ])
        }


viewAnswer : FormattedText -> Element Msg
viewAnswer answer =
    case answer of
        Paragraph str ->
            paragraph Styles.paragraph
                [ el [ Styles.font, Font.color colors.deepBlue1 ] <| text str
                ]

        ListItem str ->
            wrappedRow [ paddingEach { top = 0, right = 0, bottom = 0, left = 30 } ]
                [ paragraph Styles.paragraph
                    [ el [ Styles.font, Font.color colors.deepBlue1 ] <| text ("• " ++ str)
                    ]
                ]


collapseOrExpandBtn : String -> Int -> Element Msg
collapseOrExpandBtn icon id =
    Input.button [ alignRight, width (px 36), height (px 36), Background.color (rgb255 243 243 243), rounded 50 ]
        { onPress = Just (ToggleVisibility id)
        , label = el [ centerX, centerY, Font.size 16 ] (text icon)
        }


phoneHeader : Element Msg
phoneHeader =
    row [ width fill, Background.color colors.blue1, paddingXY 30 0 ]
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
        ]


desktopHeader : Element Msg
desktopHeader =
    row [ width fill, Background.color colors.blue1, paddingXY 100 0 ]
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
        ]


faqHeroTitlePhone : String -> Element Msg
faqHeroTitlePhone title =
    row [ width fill, Background.color colors.blue1, padding 50 ]
        [ column [ width fill, spacing 30 ]
            [ paragraph
                [ width fill
                , centerX
                , centerY
                , Font.center
                , height (minimum 50 shrink)
                , Font.color colors.white3
                , Styles.headFont
                , Font.size 40
                ]
                [ text title
                ]
            ]
        ]
