module FaqNurses.View exposing (view)

import Device exposing (Device)
import Element
    exposing
        ( Element
        , alignLeft
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , maximum
        , paddingEach
        , paddingXY
        , paragraph
        , row
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
import Html.Attributes
import Layout exposing (Layout, footer)
import Mark
import Router.Routes exposing (Page(..))
import Styles exposing (colors, css, hf, pt, wf)


view : { device : Device, model : Model, showNavMenu : Bool } -> Layout Msg
view { device, model, showNavMenu } =
    { phone =
        [ column
            [ wf
            , Font.family [ Font.typeface "Inter" ]
            , Background.color colors.cremeDark
            , css "position" "relative"
            ]
            [ row [ wf, hf, css "position" "relative" ] [ header device showNavMenu ]
            , column [ wf, hf, paddingXY 48 80 ] [ faqsView model ]
            , column [ wf, pt 120 ] footer.phone
            ]
        ]
    , tablet =
        [ column
            [ wf
            , Font.family [ Font.typeface "Inter" ]
            , css "position" "relative"
            ]
            [ row [ wf, hf ] [ header device showNavMenu ]
            , column [ wf, hf, paddingXY 48 80 ] [ faqsView model ]
            , column [ wf, pt 120 ] footer.phone
            ]
        ]
    , desktop =
        [ column
            [ wf
            , Font.family [ Font.typeface "Inter" ]
            , css "position" "relative"
            ]
            [ row [ wf, hf ] [ header device showNavMenu ]
            , column [ wf, hf, paddingXY 0 80 ] [ faqsView model ]
            , column [ wf, pt 120 ] footer.desktop
            ]
        ]
    }


header : Device.Device -> Bool -> Element Msg
header device showNavMenu =
    Layout.header
        { device = device
        , title = "FAQ for Internationally Educated Nurses"
        , navigations =
            [ ( Partnerships, "Partnerships" )
            , ( NurseCareers "", "Nurse Careers" )
            , ( Blog "", "Blog" )
            , ( About, "About" )
            ]
        , attributes =
            [ htmlAttribute <| Html.Attributes.style "position" "relative"
            , htmlAttribute <| Html.Attributes.style "background" "linear-gradient(282.96deg, #E54848 -0.52%, #BA4352 8.17%, #7E3D60 37.38%, #5D3968 66.24%)"
            ]
        , headerIconBg = Layout.HeaderIconBgBeige
        , showMenu = showNavMenu
        , toggleNavMenuMsg = ToggleNavMenu
        }


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
