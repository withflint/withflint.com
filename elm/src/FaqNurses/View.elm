module FaqNurses.View exposing (view)

import Device
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
        , fillPortion
        , height
        , html
        , htmlAttribute
        , maximum
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , spacing
        , spacingXY
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy exposing (lazy2)
import FaqNurses.Types exposing (Faq, FormattedText(..), Model, Msg(..))
import Framework.Heading as Heading
import Html
import Html.Attributes as HtmlAttr
import Layout exposing (Layout, footer, phoneMenu)
import Mark
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, css, hf, palette, pt, wf)


view : Device.Device -> Model -> Layout Msg
view device model =
    let
        render view_ =
            -- Render with phoneMenu
            if model.isPhoneMenuVisible then
                column [ wf, hf, css "position" "relative" ] [ phoneMenu PhoneMenuToggle model.isPhoneMenuVisible ]
                    |> List.singleton

            else
                view_
    in
    { phone =
        render <|
            [ column
                [ wf
                , Font.family [ Font.typeface "Inter" ]
                , Background.color colors.cremeDark
                , css "position" "relative"
                ]
                [ row [ wf, hf, css "position" "relative" ] [ header device model ]
                , column [ wf, hf, paddingXY 48 80 ] [ faqsView model ]
                , column [ wf, pt 120 ] footer.phone
                ]
            ]
    , tablet =
        render <|
            [ column
                [ wf
                , Font.family [ Font.typeface "Inter" ]
                , css "position" "relative"
                ]
                [ row [ wf, hf ] [ header device model ]
                , column [ wf, hf, paddingXY 48 80 ] [ faqsView model ]
                , column [ wf, pt 120 ] footer.phone
                ]
            ]
    , desktop =
        render <|
            [ column
                [ wf
                , Font.family [ Font.typeface "Inter" ]
                , css "position" "relative"
                ]
                [ row [ wf, hf ] [ header device model ]
                , column [ wf, hf, paddingXY 0 80 ] [ faqsView model ]
                , column [ wf, pt 120 ] footer.desktop
                ]
            ]
    }


header : Device.Device -> Model -> Element Msg
header device model =
    let
        bg =
            [ css "background" "rgb(68,55,109)"
            , css "background" "linear-gradient(282.96deg, #E54848 -0.52%, #BA4352 8.17%, #7E3D60 37.38%, #5D3968 66.24%)"
            ]

        menu =
            [ ( "Partnerships", Partnerships ), ( "Nurse Careers", NurseCareers "" ) ]

        blobSrc =
            "/static/images/header-blob-beige.svg"

        title =
            model.heroTitle

        blob =
            row [ css "position" "relative" ]
                [ row
                    [ alignTop
                    , htmlAttribute <| HtmlAttr.style "position" "relative"
                    , width (px 275)
                    , height (px 139)
                    ]
                    [ html <|
                        Html.img
                            [ HtmlAttr.src blobSrc
                            , HtmlAttr.style "width" "100%"
                            ]
                            []
                    ]
                , logo
                ]

        link : ( String, Page ) -> Element msg
        link ( label, page ) =
            Element.link
                []
                { url = toPath page
                , label =
                    el [ Font.center ] (text label)
                }

        -- responsive size
        rs =
            case device of
                Device.Phone _ ->
                    { titleFontSize = 36
                    }

                Device.Tablet _ ->
                    { titleFontSize = 32
                    }

                Device.Desktop _ ->
                    { titleFontSize = 44
                    }

                Device.NotSet ->
                    { titleFontSize = 0
                    }

        logo =
            row
                [ css "position" "absolute"
                , css "left" "44px"
                , css "top" "20px"
                , css "z-index" "100"
                ]
                [ Element.link [ wf ]
                    { url = toPath Home
                    , label =
                        Element.image
                            [ width (px 110), height (px 54) ]
                            { src = "/static/images/logo.svg?new", description = "Flint" }
                    }
                ]

        renderHamburgerMenu =
            case device of
                Device.Phone _ ->
                    phoneMenu PhoneMenuToggle model.isPhoneMenuVisible

                _ ->
                    Element.none
    in
    row ([ wf, css "position" "relative" ] ++ bg)
        [ renderHamburgerMenu
        , column [ css "position" "absolute", css "top" "0", css "left" "0" ]
            [ row [ css "width" "80%", css "height" "80%" ] [ blob ]
            ]
        , column
            [ alignTop, height (px 280), wf ]
            [ -- GAP
              case device of
                Device.Phone _ ->
                    row [ wf, height <| fillPortion 4 ] [ Element.none ]

                _ ->
                    row [ wf, height <| fillPortion 4 ]
                        [ -- MENU
                          row [ wf ]
                            [ -- GAP
                              row [ width <| fillPortion 7 ] []

                            -- MENU
                            , row
                                [ width <| fillPortion 4
                                , spacing 32
                                , Font.color palette.white
                                , Font.letterSpacing 2
                                , Font.size 14
                                ]
                                [ row [ alignRight, spacingXY 36 0 ]
                                    (List.map (el (wf :: Styles.menu) << link) menu)
                                ]

                            -- GAP
                            , row [ width <| fillPortion 2 ] []
                            ]
                        ]

            -- TITLE
            , row [ wf, height <| fillPortion 8 ]
                [ el ([ wf, centerX, Font.size rs.titleFontSize ] ++ Styles.title ++ Heading.h1)
                    (paragraph [ Font.center, Font.size rs.titleFontSize ] [ text title ])
                ]

            -- GAP
            , case device of
                Device.Phone _ ->
                    Element.none

                _ ->
                    row [ wf, height <| fillPortion 2 ] []
            ]
        ]


faqsView : Model -> Element Msg
faqsView { faqs } =
    column
        [ Background.color colors.cremeDark
        , wf
        , hf
        , Font.family [ Font.typeface "Inter" ]
        ]
        [ column [ width <| maximum 850 fill, height fill, spacingXY 0 40, centerX ]
            (List.indexedMap (lazy2 viewAFaq) faqs)
        ]


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
            [ paragraph [ width fill, alignLeft, Font.medium, Styles.headFont ] (Mark.default faq.question)
            ]
        , column
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
        ]


viewAnswer : FormattedText -> Element Msg
viewAnswer answer =
    case answer of
        Paragraph str ->
            paragraph Styles.paragraph
                (Mark.default str)

        ListItem str ->
            wrappedRow [ paddingEach { top = 0, right = 0, bottom = 0, left = 30 } ]
                [ paragraph Styles.paragraph
                    [ el [ Styles.font ] <| text ("• " ++ str)
                    ]
                ]

        OrderedItem ( number, str ) ->
            wrappedRow [ paddingEach { top = 0, right = 0, bottom = 0, left = 30 } ]
                [ paragraph Styles.paragraph
                    [ el [ Styles.font ] <| text (String.fromInt number ++ ". " ++ str)
                    ]
                ]
