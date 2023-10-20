module NurseCareers.View exposing (config)

import Device exposing (Device)
import Element
    exposing
        ( Element
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
        , mouseOver
        , none
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , spacingXY
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Layout exposing (Layout)
import NurseCareers.Types exposing (Model, Msg(..))
import Router.Routes exposing (Page(..))
import Styles exposing (colors)


config : { device : Device, model : Model, showNavMenu : Bool } -> Layout Msg
config props =
    { phone = List.singleton <| view props
    , tablet = List.singleton <| view props
    , desktop = List.singleton <| view props
    }


view : { device : Device, model : Model, showNavMenu : Bool } -> Element Msg
view { device, model, showNavMenu } =
    column
        [ width fill ]
        [ header device showNavMenu
        , content device model
        ]


header : Device -> Bool -> Element Msg
header device showNavMenu =
    Layout.header
        { device = device
        , title = "Your success is Flint's success"
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


content : Device -> Model -> Element Msg
content device model =
    let
        title =
            paragraph
                [ Font.center
                , Font.size 28
                , Font.semiBold
                , Font.color colors.primary
                , paddingXY 0 48
                ]
                [ text "We are committed to your nursing future in the USA"
                ]

        context =
            let
                wrapper =
                    case device of
                        Device.Desktop _ ->
                            row [ width fill, spacingXY 36 0 ]

                        Device.Phone _ ->
                            column [ width fill, spacingXY 0 50 ]

                        Device.Tablet _ ->
                            row [ width fill, spacingXY 36 0 ]

                        Device.NotSet ->
                            row [ width fill, spacingXY 36 0 ]
            in
            wrapper
                [ paragraph
                    [ alignTop
                    , htmlAttribute <| Html.Attributes.style "line-height" (String.fromFloat 1.6)
                    ]
                    [ text "Flint is an international search firm seeking experienced and qualified nurses from around the world. Our program is specifically designed to help internationally educated nurses succeed permanently in the United States." ]
                , paragraph
                    [ alignTop
                    , htmlAttribute <| Html.Attributes.style "line-height" (String.fromFloat 1.6)
                    ]
                    [ text "We partner with respected American hospitals.  We offer an all-inclusive solution for nurses to seamlessly transition into their new life in America. Flint provides fully sponsored licensing, immigration, and relocation programs. We pay for legal and processing fees, licensing, and offer premium placement. "
                    , Element.link
                        [ width fill ]
                        { url = "/internationally-educated-nurses-faq/"
                        , label =
                            paragraph
                                [ Font.justify, Font.underline, Font.color colors.primary ]
                                [ text "Learn more." ]
                        }
                    ]
                ]

        advantages =
            let
                advantage c label =
                    column [ width fill ]
                        [ Element.image [ centerX, width (px 72), height (px 88) ] c
                        , paragraph
                            [ Font.center
                            , Font.color colors.primary
                            , Font.semiBold
                            ]
                            [ label ]
                        ]
            in
            (case device of
                Device.Phone _ ->
                    wrappedRow [ centerX, spacingXY 20 32, paddingXY 0 50 ]

                _ ->
                    wrappedRow [ centerX, spacingXY 120 32, paddingXY 0 50 ]
            )
                [ advantage { src = "/static/images/licensing.svg", description = "Flint - Licensing" } (text "Licensing")
                , advantage { src = "/static/images/immigration.svg", description = "Flint - Immigration" } (text "Immigration")
                , advantage { src = "/static/images/relocation.svg", description = "Flint - Relocation" } (text "Relocation")
                ]

        applyNow =
            paragraph
                [ Font.center
                , Font.size 28
                , Font.semiBold
                , Font.color colors.primary
                , paddingXY 0 48
                ]
                [ text "Apply Now!"
                ]

        nurseSuccessInfo =
            let
                video =
                    el
                        [ height fill
                        , width (fillPortion 1)
                        , Border.color colors.primary
                        ]
                    <|
                        html <|
                            Html.video
                                [ Html.Attributes.style "width" "100%"
                                , Html.Attributes.style "height" "100%"
                                , Html.Attributes.controls True
                                ]
                                [ Html.source [ Html.Attributes.src "/static/videos/nurse-success.mp4" ] [] ]
            in
            (case device of
                Device.Phone _ ->
                    column [ centerX, spacingXY 20 32, paddingXY 0 50 ]

                _ ->
                    wrappedRow [ width fill, spacingXY 64 0, paddingEach { top = 64, bottom = 48, right = 0, left = 0 } ]
            )
                [ video
                , column
                    [ width <| fillPortion 1
                    , spacingXY 0 24
                    ]
                    [ paragraph
                        [ Font.alignLeft
                        , Font.size 26
                        , Font.semiBold
                        , Font.color colors.primary
                        ]
                        [ text "From start to finish" ]
                    , paragraph
                        [ Font.alignLeft
                        , htmlAttribute <| Html.Attributes.style "line-height" (String.fromFloat 1.6)
                        ]
                        [ text "Our talented team of nurse educators and staff will guide you through the entire process. Flint offers an NCLEX preparation course, covers the cost of taking the NCLEX, provides travel to the nearest testing center, completes your nurse license application, provides job placement, and world-class immigration services. We consider your nursing skills, experience, and goals when assessing which facilities are best suited for you." ]
                    ]
                ]

        applyButton =
            Input.button
                [ Border.rounded 2
                , Font.color colors.white
                , Font.size 20
                , Background.color colors.carminePink
                , paddingEach { top = 11, right = 19, bottom = 11, left = 22 }
                , Font.regular
                , mouseOver
                    [ Font.color colors.cremeLight
                    , Background.color colors.carminePink
                    , Border.color colors.carminePink
                    ]
                , alignRight
                ]
                { onPress = Just ApplyButtonClicked
                , label = text "Apply →"
                }

        applicationText =
            paragraph
                [ Font.justify
                , Font.alignLeft
                ]
                [ text "Apply now and discover what exciting new career opportunities with growth potential awaits you in America, where you will apply your existing skills and knowledge while learning new ones. Our team of experienced nurse educators will guide you and start your journey today." ]

        application =
            column
                [ width fill, paddingXY 0 20 ]
                [ applyButton
                ]

        partnerText =
            column [ width fill, paddingXY 28 100, spacingXY 0 24, centerX ]
                [ paragraph [ Font.center, Font.size 28, Font.color colors.primary, centerY ] [ text "We partner with the most trusted names in the business." ]
                , paragraph [ centerY, centerX, Font.center, width (fill |> maximum 600) ] [ text "Flint's industry partnerships mean the highest standards in nurse quality and competency." ]
                ]

        margin n =
            el [ width <| fillPortion n ] none
    in
    column
        [ width fill
        , htmlAttribute <| Html.Attributes.style "background" "#FCE5D9"
        , htmlAttribute <| Html.Attributes.style "background" "linear-gradient(180deg, #FFFBF8 0%, #FCE5D9 102.99%)"
        ]
        [ row
            [ width fill
            , paddingXY 0 100
            ]
            [ margin 1
            , column
                [ width <| fillPortion 4 ]
                [ title
                , context
                , advantages
                , nurseSuccessInfo
                , case device of
                    Device.Phone _ ->
                        column [ width fill ]
                            [ applyNow
                            , applicationText
                            , application
                            ]

                    _ ->
                        row
                            [ width fill ]
                            [ margin 2
                            , column [ width (fillPortion 3) ]
                                [ applyNow
                                , applicationText
                                , application
                                ]
                            , margin 2
                            ]
                ]
            , margin 1
            ]
        , Layout.partners device
        , partnerText
        , column [ width fill ] <| Layout.footer_ device
        ]
