module FaqNurses.View exposing (..)

import Element
    exposing
        ( Element
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
        , maximum
        , minimum
        , mouseOver
        , newTabLink
        , none
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , shrink
        , spacing
        , spacingXY
        , text
        , textColumn
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FaqNurses.Types exposing (Model, Msg(..))
import Layout exposing (Layout, footer, header, menu)
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
            (faqView :: footer.phone)
        ]
    , tablet =
        [ desktopHeader
        , faqHeroTitlePhone model.heroTitle
        , column
            [ width fill
            , height fill
            , paddingXY 20 40
            , spacing 50
            , centerX
            ]
            (faqView :: footer.desktop)
        ]
    , desktop =
        [ desktopHeader
        , faqHeroTitlePhone model.heroTitle
        , column
            [ width fill
            , height fill
            , paddingXY 20 40
            , spacing 50
            , centerX
            ]
            (faqView :: footer.desktop)
        ]
    }


faqView : Element Msg
faqView =
    Element.el [] none


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


faqHeroTitleDesktop : String -> Element Msg
faqHeroTitleDesktop title =
    row [ width fill, Background.color colors.blue1, padding 50 ]
        [ column [ width fill, spacing 30 ]
            [ paragraph
                [ width <| maximum 1400 fill
                , centerX
                , centerY
                , Font.center
                , height (minimum 150 shrink)
                , Font.color colors.white3
                , Styles.headFont
                , Font.size 70
                ]
                [ text title
                ]
            ]
        ]
