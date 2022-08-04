module Home.View exposing (view)

import Device exposing (Device(..))
import Element
    exposing
        ( Attribute
        , Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , html
        , maximum
        , mouseOver
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Home.Types exposing (Model)
import Html
import Html.Attributes as HtmlAttr
import Layout exposing (Layout, footer)
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, css, hf, palette, pl, pt)


view : Model -> Device -> Layout msg
view _ device =
    { phone =
        [ column
            [ width <| maximum 1500 fill
            , height fill
            , centerX
            ]
            (phoneView device
                ++ footer.phone
            )
        ]
    , tablet =
        [ column
            [ width <| maximum 1500 fill
            , height fill
            , wf
            ]
            (desktopView device
                ++ footer.phone
            )
        ]
    , desktop =
        [ column
            [ centerX
            , wf
            , height fill
            ]
            (desktopView device
                ++ footer.desktop
            )
        ]
    }


phoneView : Device -> List (Element msg)
phoneView device =
    let
        viewport =
            case device of
                Phone vp ->
                    vp

                Tablet vp ->
                    vp

                Desktop vp ->
                    vp

                NotSet ->
                    { width = 0, height = 0 }

        heroImg =
            html <|
                Html.div
                    []
                    [ Html.div
                        [ HtmlAttr.style "position" "relative"
                        , HtmlAttr.style "width" "100vw"
                        , HtmlAttr.style "overflow" "hidden"
                        ]
                        [ Html.img
                            [ HtmlAttr.src "/static/images/home-hero-blob.svg"
                            , HtmlAttr.style "width" (String.fromInt viewport.width)
                            , HtmlAttr.style "position" "absolute"
                            , HtmlAttr.style "bottom" (String.fromInt viewport.width)
                            , HtmlAttr.style "right" "-40px"
                            , HtmlAttr.style "z-index" "1"
                            ]
                            []
                        , Html.div
                            [ HtmlAttr.style "display" "flex"
                            , HtmlAttr.style "justify-content" "center"
                            ]
                            [ Html.img
                                [ HtmlAttr.src "/static/images/home-portrait-nurse.png"
                                , HtmlAttr.style "width" "inherit"
                                , HtmlAttr.style "z-index" "2"
                                ]
                                []
                            ]
                        ]
                    ]

        logo : Element msg
        logo =
            Element.image [ width (px 110), height (px 54) ] { src = "/static/images/logo.svg?new", description = "Flint" }
    in
    [ column [ wf, hf, Background.color colors.cremeDark, pt 36 ]
        [ -- HERO
          column [ wf ]
            [ -- HERO TEXT
              column [ wf, spacingXY 0 14, pl 24 ]
                [ paragraph
                    [ Font.color palette.primary
                    , Font.semiBold
                    , Font.size 42
                    ]
                    [ text "It's all about people, with" ]
                , logo
                ]

            -- HERO IMG
            , heroImg
            ]

        -- CARD
        , column
            [ wf ]
            [ card device
                { title = "Need a long-term nurse?"
                , desc = "Recreate the way you hire nurses"
                , btn = { label = "Flint for hospitals", page = Partnerships }
                , bg = industryBg
                }
            , card device
                { title = "Want to be a nurse in America?"
                , desc = "Find support and community from start to finish"
                , btn = { label = "Flint for nurses", page = NurseCareers "" }
                , bg = nursesBg
                }
            ]
        ]
    ]


card :
    Device
    ->
        { title : String
        , desc : String
        , btn : { label : String, page : Page }
        , bg : List (Attribute msg)
        }
    -> Element msg
card device { title, desc, btn, bg } =
    let
        responsiveSize =
            case device of
                Phone _ ->
                    { cardHeight = 200, btnTopPadding = 24 }

                Desktop _ ->
                    { cardHeight = 300, btnTopPadding = 44 }

                Tablet _ ->
                    { cardHeight = 300, btnTopPadding = 44 }

                NotSet ->
                    { cardHeight = 300, btnTopPadding = 44 }

        btn_ =
            [ Border.roundEach { topLeft = 16, topRight = 0, bottomRight = 16, bottomLeft = 0 }
            , padding 10
            , Font.color palette.primary
            , Font.semiBold
            , Font.size 16
            , Background.color colors.cremeDark
            , paddingEach { top = 10, right = 19, bottom = 10, left = 22 }
            , Font.regular
            , mouseOver
                [ Font.color colors.cremeLight
                , Background.color colors.carminePink
                , Border.color colors.carminePink
                ]
            ]
    in
    column
        ([ wf, paddingXY 0 32 ]
            ++ bg
        )
        [ column
            [ height (px responsiveSize.cardHeight)
            , Font.color palette.white
            , centerX
            ]
            [ column [ wf, centerX, centerY ]
                [ column [ pl 24 ]
                    [ paragraph
                        [ Font.size 26
                        , Font.semiBold
                        ]
                        [ text <| title ]
                    , paragraph
                        [ Font.family [ Font.typeface "Inter" ]
                        , Font.size 16
                        , pt 12
                        ]
                        [ text <| desc ]
                    , Element.link [ wf ]
                        { url = toPath btn.page
                        , label =
                            el [ pt responsiveSize.btnTopPadding, wf ]
                                (Input.button
                                    (centerY :: centerX :: wf :: Font.size 15 :: btn_)
                                    { onPress = Nothing
                                    , label = paragraph [ Font.center ] [ text <| btn.label ]
                                    }
                                )
                        }
                    ]
                ]
            ]
        ]


industryBg : List (Attribute msg)
industryBg =
    [ css "background" "rgb(68,55,109)"
    , css "background" "linear-gradient(281.17deg, #A7C8F9 -8.91%, #8494C7 12.48%, #6E74A9 42.43%, #626297 82.36%)"
    ]


nursesBg : List (Attribute msg)
nursesBg =
    [ css "background" "rgb(229,72,72)"
    , css "background" "linear-gradient(282.96deg, #E54848 -0.52%, #BA4352 8.17%, #7E3D60 37.38%, #5D3968 66.24%)"
    ]


desktopView : Device -> List (Element msg)
desktopView device =
    let
        heroImg =
            row
                [ centerX
                , css "position" "relative"
                , css "overflow" "hidden"
                ]
                [ column
                    [ wf
                    , css "z-index" "2"
                    ]
                    [ paragraph heroTitleAttr [ text "It's all about people," ]
                    , row []
                        [ paragraph heroTitleAttr [ text "with" ]
                        , Element.image [ width (px 114), height (px 48) ] { src = "/static/images/logo.svg?new", description = "Flint" }
                        ]
                    ]
                , Element.image
                    [ css "z-index" "3"
                    ]
                    { src =
                        "/static/images/home-portrait-nurse.png"
                    , description = "Flint"
                    }
                , html <|
                    Html.img
                        [ HtmlAttr.src "/static/images/home-hero-blob.svg"
                        , HtmlAttr.style "width" "90%"
                        , HtmlAttr.style "position" "absolute"
                        , HtmlAttr.style "bottom" "-140px"
                        , HtmlAttr.style "right" "-16px"
                        , HtmlAttr.style "z-index" "1"
                        ]
                        []
                ]

        heroTitleAttr =
            [ Font.color palette.primary
            , Font.semiBold
            , Font.size 42
            ]

        link label page =
            Element.link
                (Styles.menu
                    ++ [ Font.color palette.primary ]
                )
                { url = toPath page
                , label =
                    Element.paragraph [ Font.center ] [ text label ]
                }
    in
    [ column [ pt 48, wf, hf, Background.color colors.cremeDark ]
        [ -- row [ spacing 48, centerX, Font.color palette.primary, Font.semiBold ]
          -- [ link "Partnerships" Partnerships
          -- , link "Nurse Careers" (NurseCareers "")
          -- ]
          row [ pt 72, wf ]
            [ heroImg
            ]
        , row [ wf, Background.color colors.blue1 ]
            [ card device
                { title = "Need a long-term nurse?"
                , desc = "Recreate the way you hire nurses"
                , btn = { label = "Flint for hospitals", page = Partnerships }
                , bg = industryBg
                }
            , card device
                { title = "Want to be a nurse in America?"
                , desc = "Find support and community from start to finish"
                , btn = { label = "Flint for nurses", page = NurseCareers "" }
                , bg = nursesBg
                }
            ]
        ]
    ]


wf : Element.Attribute msg
wf =
    width fill
