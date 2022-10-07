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
        , fillPortion
        , height
        , html
        , maximum
        , mouseOver
        , paddingXY
        , paragraph
        , px
        , row
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Home.Types exposing (Model, Msg(..))
import Html
import Html.Attributes as HtmlAttr
import Layout exposing (Layout, footer, phoneMenu, topMenu)
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, css, hf, pl, pt, wf)


view : Model -> Device -> Layout Msg
view model device =
    { phone =
        [ column
            [ wf
            , height fill
            , css "position" "relative"
            ]
          <|
            phoneView device model
        ]
    , tablet =
        [ column
            [ width <| maximum 1500 fill
            , height fill
            , wf
            ]
            (header device
                ++ desktopView device
                ++ footer.phone
            )
        ]
    , desktop =
        [ column
            [ centerX
            , wf
            , height fill
            ]
            (header device
                ++ desktopView device
                ++ footer.desktop
            )
        ]
    }


phoneView : Device -> Model -> List (Element Msg)
phoneView device model =
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
    -- Phone Menu
    if model.isPhoneMenuVisible then
        [ phoneMenu PhoneMenuToggle model.isPhoneMenuVisible
        ]

    else
        column [ wf, hf, Background.color colors.cremeDark, pt 36, css "position" "relative" ]
            [ -- HERO
              column [ wf ]
                [ -- Hamburger Menu
                  phoneMenu PhoneMenuToggle model.isPhoneMenuVisible

                -- HERO TEXT
                , column [ wf, spacingXY 0 14, pl 24 ]
                    [ paragraph
                        [ Font.color colors.primary
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
                [ -- apt name would be section instead of card
                  card device
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
            -- FOOTER
            :: footer.phone


header : Device.Device -> List (Element msg)
header _ =
    let
        bg =
            [ css "background" "#DAE9FF"
            , css "background" "linear-gradient(275.4deg, #E54848 -14.67%, #7B3D61 14.83%, #51497F 55.96%, #616297 92.44%, #A7C8F9 127.36%)"
            ]

        logo =
            Element.image [ centerX, width (px 100), height (px 50) ] { src = "/static/images/logo-white.svg?new", description = "Flint" }
    in
    [ row ([ wf, height (px 136) ] ++ bg)
        [ row [ width <| fillPortion 2 ] []
        , row [ width <| fillPortion 1 ]
            [ Element.link
                []
                { url = toPath Home
                , label =
                    el
                        [ Font.center
                        , Font.color colors.white
                        , mouseOver [ Font.color colors.carminePink ]
                        ]
                        logo
                }
            ]
        , row [ width <| fillPortion 8 ] []
        , row [ width <| fillPortion 2, spacingXY 24 0 ]
            (topMenu
                |> List.map
                    (\( label, page ) ->
                        Element.link
                            []
                            { url = toPath page
                            , label =
                                el Styles.menu (text label)
                            }
                    )
            )
        , row [ width <| fillPortion 2 ] []
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

        btnConfig =
            { fontColor = colors.primary
            , bgColor = colors.cremeDark
            }
    in
    column
        ([ wf, paddingXY 0 32 ]
            ++ bg
        )
        [ column
            [ height (px responsiveSize.cardHeight)
            , Font.color colors.white
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
                                    (centerY
                                        :: centerX
                                        :: wf
                                        :: Font.size 15
                                        :: Styles.btnFilled btnConfig
                                    )
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
            [ Font.color colors.primary
            , Font.semiBold
            , Font.size 42
            ]
    in
    [ column [ pt 48, wf, Background.color colors.cremeDark ]
        [ row [ pt 72, wf ]
            [ heroImg
            ]
        , row [ wf, Background.color colors.cremeDark ]
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
