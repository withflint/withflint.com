module Jobs.View exposing (view)

import Device
import Dict
import Element
    exposing
        ( Attribute
        , Element
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , focused
        , height
        , html
        , htmlAttribute
        , link
        , maximum
        , minimum
        , mouseOver
        , none
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , spaceEvenly
        , spacing
        , spacingXY
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File
import Framework.Heading as Heading
import Html
import Html.Attributes as HtmlAttr
import Jobs.Types exposing (Config, CurrentPage(..), Field(..), Job, Model, Msg(..), View(..))
import Layout exposing (Layout, footer, menu, phoneMenu)
import Mark
import RemoteData exposing (RemoteData(..))
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, css, hf, lineHeight, minH, minW, palette, pt, wp)
import Text
import Url.Builder exposing (absolute)


view : Device.Device -> Model -> Layout Msg
view device model =
    let
        sectionBg =
            [ css "background" "#DAE9FF"
            , css "background" "linear-gradient(180deg, #FFFBF8 0%, #DAE9FF 102.99%)"
            ]

        render view_ =
            -- Render with phoneMenu
            if model.isPhoneMenuVisible then
                column [ wf, hf, css "position" "relative" ] [ phoneMenu PhoneMenuToggle model.isPhoneMenuVisible ]
                    |> List.singleton

            else
                view_
    in
    case model.view of
        JobsView ->
            { phone =
                render <|
                    [ column
                        [ wf
                        , Font.family [ Font.typeface "Inter" ]
                        ]
                        [ row [ wf, hf ] [ toHeader device model ]
                        , toView device model.config
                        ]

                    -- JobView
                    , column
                        ([ wf
                         , height fill
                         , Font.family [ Font.typeface "Inter" ]
                         ]
                            ++ sectionBg
                        )
                        [ jobsView device phoneView model ]
                    , column [ wf ] footer.phone
                    ]
            , desktop =
                render <|
                    [ -- top header including hero title and nav bar
                      column
                        [ wf
                        , Font.family [ Font.typeface "Inter" ]
                        ]
                        [ row [ wf, hf ] [ toHeader device model ]
                        , toView device model.config
                        ]

                    -- JobView
                    , column
                        ([ wf
                         , height fill
                         , Font.family [ Font.typeface "Inter" ]
                         , centerX
                         ]
                            ++ sectionBg
                        )
                        [ jobsView device desktopView model ]
                    , column [ wf ] footer.desktop
                    ]
            , tablet =
                render <|
                    [ column
                        [ wf
                        , Font.family [ Font.typeface "Inter" ]
                        ]
                        [ row [ wf, hf ] [ toHeader device model ]
                        , toView device model.config
                        ]

                    -- JobView
                    , column
                        ([ wf
                         , height fill
                         , Font.family [ Font.typeface "Inter" ]
                         , centerX
                         ]
                            ++ sectionBg
                        )
                        [ jobsView device desktopView model ]
                    , column [ wf ] footer.phone
                    ]
            }

        ApplyView _ ->
            { phone =
                render <|
                    [ column
                        [ wf
                        , Font.family [ Font.typeface "Inter" ]
                        ]
                        [ toHeader device model
                        ]
                    , column
                        ([ wf
                         , height fill
                         , Font.family [ Font.typeface "Inter" ]
                         , paddingXY 20 60
                         , centerX
                         ]
                            ++ sectionBg
                        )
                        [ jobsView device phoneView model ]
                    , column [ wf ] footer.phone
                    ]
            , desktop =
                render <|
                    [ -- top header including hero title and nav bar
                      column
                        [ wf
                        , Font.family [ Font.typeface "Inter" ]
                        ]
                        [ toHeader device model
                        ]
                    , column
                        ([ wf
                         , height fill
                         , paddingXY 0 80
                         , centerX
                         , Font.family [ Font.typeface "Inter" ]
                         ]
                            ++ sectionBg
                        )
                        [ jobsView device desktopView model ]
                    , column [ wf ] footer.desktop
                    ]
            , tablet =
                render <|
                    [ column
                        [ wf
                        , Font.family [ Font.typeface "Inter" ]
                        ]
                        [ toHeader device model
                        ]
                    , column
                        ([ wf
                         , height fill
                         , paddingXY 20 80
                         , Font.family [ Font.typeface "Inter" ]
                         , centerX
                         ]
                            ++ sectionBg
                        )
                        [ jobsView device desktopView model ]
                    , column [ wf ] footer.phone
                    ]
            }



----- #### View Functions #### -------


toView : Device.Device -> Config -> Element msg
toView device config =
    case config.page_ of
        NurseCareersPage ->
            nurseCareerView device

        JoinTheTeamPage ->
            joinTeamView device



----- #### Nurse Career #### -------


nurseCareerView : Device.Device -> Element msg
nurseCareerView device =
    let
        sectionBg =
            [ css "background" "#FCE5D9"
            , css "background" "linear-gradient(180deg, #FFFBF8 0%, #FCE5D9 102.99%)"
            ]
    in
    column
        [ Background.color colors.cremeDark
        , wf
        , hf
        , Font.family [ Font.typeface "Inter" ]
        ]
        [ row (wf :: sectionBg)
            [ row [ width <| fillPortion 2 ] [ Element.none ]
            , column [ width <| fillPortion 8 ] [ nurseCareerBody device ]
            , row [ width <| fillPortion 2 ] [ Element.none ]
            ]
        , partners device
        , states device
        ]


states : Device.Device -> Element msg
states device =
    let
        titleStyle =
            [ Font.center
            , Font.size 24
            , Font.semiBold
            , Font.color colors.primary
            ]

        rsFillPortion =
            -- responsive fillPortion
            case device of
                Device.Phone _ ->
                    Element.none

                _ ->
                    row [ width <| fillPortion 2 ] []
    in
    row [ wf, paddingXY 12 56 ]
        [ rsFillPortion
        , column [ width <| fillPortion 8, spacingXY 0 48 ]
            [ paragraph titleStyle [ text "US states where you can live and work" ]
            , wrappedRow
                [ wf
                , Font.color colors.primary
                , spaceEvenly
                , spacingXY 12 32
                ]
                [ column [ centerX, width <| fillPortion 3, spacingXY 0 22 ]
                    [ paragraph [ Font.center ] [ text "Alaska" ]
                    , paragraph [ Font.center ] [ text "Arizona" ]
                    , paragraph [ Font.center ] [ text "Colorado" ]
                    , paragraph [ Font.center ] [ text "Delaware" ]
                    , paragraph [ Font.center ] [ text "Georgia" ]
                    ]
                , column [ centerX, width <| fillPortion 3, spacingXY 0 22 ]
                    [ paragraph [ Font.center ] [ text "Iowa" ]
                    , paragraph [ Font.center ] [ text "Maryland" ]
                    , paragraph [ Font.center ] [ text "Missouri" ]
                    , paragraph [ Font.center ] [ text "Nebraska" ]
                    , paragraph [ Font.center ] [ text "New Jersey" ]
                    ]
                , column [ centerX, width <| fillPortion 3, spacingXY 0 22 ]
                    [ paragraph [ Font.center ] [ text "New Mexico" ]
                    , paragraph [ Font.center ] [ text "New York" ]
                    , paragraph [ Font.center ] [ text "Ohio" ]
                    , paragraph [ Font.center ] [ text "Pennsylvania" ]
                    , paragraph [ Font.center ] [ text "South Carolina" ]
                    ]
                , column [ centerX, width <| fillPortion 3, spacingXY 0 22 ]
                    [ paragraph [ Font.center ] [ text "Tennessee" ]
                    , paragraph [ Font.center ] [ text "Texas" ]
                    , paragraph [ Font.center ] [ text "Virginia" ]
                    , paragraph [ Font.center ] [ text "Washington" ]
                    , paragraph [ Font.center ] [ text "Wisconsin" ]
                    ]
                ]
            ]
        , rsFillPortion
        ]


nurseCareerBody : Device.Device -> Element msg
nurseCareerBody device =
    let
        titleStyle =
            [ Font.center
            , Font.size 28
            , Font.semiBold
            , Font.color colors.primary
            ]

        btnConfig =
            { fontColor = colors.white
            , bgColor = colors.carminePink
            }

        rsJustify =
            case device of
                Device.Phone _ ->
                    Font.center

                _ ->
                    Font.justify
        rsDiv =
            case device of
                Device.Phone _ -> column
                _ -> row
    in
    column [ wf, centerX, paddingXY 0 48, spacingXY 0 48 ]
        [ column [ centerX ]
            [ paragraph titleStyle
                [ text "We are committed to your nursing future in the USA" ]
            ]
        , rsDiv [ spacingXY 34 0, alignTop, spacingXY 40 48]
            [ paragraph [ alignTop, Font.center, pt 12, rsJustify, lineHeight 1.6]
                [ text "Flint is an international search firm seeking experienced and qualified nurses from around the world. Our program is specifically designed to help internationally educated nurses succeed permanently in the United States." ]
            , paragraph
                [ Font.center
                , alignTop
                , pt 12
                , rsJustify
                , lineHeight 1.6
                ]
                [ text "We partner with respected American hospitals.  We offer an all-inclusive solution for nurses to seamlessly transition into their new life in America. Flint provides fully sponsored licensing, immigration, and relocation programs. We pay for legal and processing fees, licensing, and offer premium placement. "
                , Element.link
                    [ wf ]
                    { url = "/internationally-educated-nurses-faq/"
                    , label = paragraph [ rsJustify, Font.underline, Font.color colors.primary ] [ text "Learn more." ]
                    }
                ]
            ]
        , column [ wf, spacingXY 0 44, pt 24 ]
            [ advantages device
            , column [ spacingXY 0 24, wf, centerX ]
                [ row [ centerX ]
                    [ el [ wf ]
                        (link
                            (centerY :: centerX :: wf :: Font.size 15 :: Styles.btnFilled btnConfig)
                            { url = "/nurse-careers/general-health-care-application-rn-np-lpn-hsp-anywhere-usa"
                            , label = paragraph [ Font.center ] [ text <| "Apply" ]
                            }
                        )
                    ]
                ]
            , nurseSuccessInfo
            ]
        ]


advantages : Device.Device -> Element msg
advantages device =
    wrappedRow [ centerX, spacingXY 64 32 ]
        [ column [ spacingXY 0 24, minW 160 ]
            [ Element.image [ centerX, width (px 72), height (px 87) ] { src = "/static/images/licensing.svg", description = "Flint - Licensing" }
            , paragraph [ Font.center, Font.color colors.primary, Font.semiBold ] [ text "Licensing" ]
            ]
        , column [ spacingXY 0 24, minW 160 ]
            [ Element.image [ centerX, width (px 72), height (px 87) ] { src = "/static/images/immigration.svg", description = "Flint - Immigration" }
            , paragraph [ Font.center, Font.color colors.primary, Font.semiBold ] [ text "Immigration" ]
            ]
        , column [ spacingXY 0 24, minW 160 ]
            [ Element.image [ centerX, width (px 72), height (px 87) ] { src = "/static/images/relocation.svg", description = "Flint - Relocation" }
            , paragraph [ Font.center, Font.color colors.primary, Font.semiBold ] [ text "Relocation" ]
            ]
        ]


nurseSuccessInfo : Element msg
nurseSuccessInfo =
    let
        video =
            row [wf, hf, minW 340]
            [ html <|
                Html.video [ HtmlAttr.style "width" "100%", HtmlAttr.style "height" "100%", HtmlAttr.controls True 
                    ]
                    [ Html.source [ HtmlAttr.src "/static/videos/nurse-success.mp4" ] []
                    ]
            ]

        subHeading =
            [ Font.size 26
            , Font.semiBold
            , Font.color colors.primary
            ]
    in
    wrappedRow [ wf, paddingEach { top = 64, bottom = 48, right = 0, left = 0 }, spacingXY 96 40 ]
        [ row [ width <| fillPortion 6, Border.color colors.primary ] [ 
            video 
            ]
        , column [ width <| (fillPortion 6 |> Element.minimum 300), spacingXY 0 24 ]
            [ paragraph (Font.alignLeft :: subHeading) [ text "From start to finish" ]
            , paragraph
                [ Font.alignLeft
                , lineHeight 1.6
                , pt 12
                ]
                [ text "Our talented team of nurse educators and staff will guide you through the entire process. Flint offers an NCLEX preparation course, covers the cost of taking the NCLEX, provides travel to the nearest testing center, completes your nurse license application, provides job placement, and world-class immigration services. We consider your nursing skills, experience, and goals when assessing which facilities are best suited for you." ]
            ]
        ]


partners : Device.Device -> Element msg
partners device =
    let
        bgBlue =
            [ css "background" "#5C4B92"
            , css "background" "linear-gradient(90deg, #50417F 0%, #5C4B92 100%)"
            ]

        rsPortion =
            case device of
                Device.Phone _ ->
                    { row1 = wf
                    , row2 = wf
                    , row3 = wf
                    , spacing = spacingXY 0 32
                    , bg = Background.color colors.white
                    }

                Device.Desktop _ ->
                    { row1 = wp 2
                    , row2 = wp 8
                    , row3 = wp 2
                    , spacing = spaceEvenly
                    , bg = Background.color palette.cremeLight
                    }

                Device.Tablet _ ->
                    { row1 = wp 0
                    , row2 = wp 12
                    , row3 = wp 0
                    , spacing = spaceEvenly
                    , bg = Background.color palette.cremeLight
                    }

                Device.NotSet ->
                    { row1 = wp 1
                    , row2 = wp 10
                    , row3 = wp 1
                    , spacing = spaceEvenly
                    , bg = Background.color palette.cremeLight
                    }
    in
    column [ wf ]
        [ wrappedRow
            ([ wf
             , hf
             ]
                ++ bgBlue
            )
            [ row [ rsPortion.row1 ] []
            , wrappedRow
                [ rsPortion.row2
                , paddingXY 32 128
                , rsPortion.spacing
                ]
                [ column [ spacingXY 0 24, alignTop ]
                    [ row [ width (px 210), height (px 75) ]
                        [ Element.image [ centerX, css "width" "100%" ] { src = "/static/images/cgfns-logo.svg", description = "CGFNS International" }
                        ]
                    , column [ spacingXY 12 12 ]
                        [ row [ width (px 95), height (px 83) ]
                            [ Element.image [ centerX, css "width" "100%" ] { src = "/static/images/jsa-logo.svg", description = "JSA" }
                            ]
                        , column [ wf, Font.color colors.white, Font.size 12 ]
                            [ paragraph [] [ text "Josef Silny & Associates, Inc." ]
                            , paragraph [] [ text "International Education Consultants" ]
                            ]
                        ]
                    ]
                , column [ spacingXY 0 48, alignTop ]
                    [ row [ width (px 224), height (px 95) ]
                        [ Element.image [ centerX, css "width" "100%" ] { src = "/static/images/hca-logo.svg", description = "HCA Healthcare" }
                        ]
                    , row [ width (px 264), height (px 65) ]
                        [ Element.image [ centerX, css "width" "100%" ] { src = "/static/images/medall-logo.svg", description = "MedAll" }
                        ]
                    ]
                , column [ spacingXY 0 48 ]
                    [ row [ width (px 198), height (px 52) ]
                        [ Element.image [ centerX, css "width" "100%" ] { src = "/static/images/ringmd-logo.svg", description = "RingMd" }
                        ]
                    , row [ width (px 264), height (px 65) ]
                        [ Element.image [ centerX, css "width" "100%" ] { src = "/static/images/learn-with-nurses-logo.svg", description = "Learn with Nurses" }
                        ]
                    ]
                ]
            , row [ rsPortion.row3 ] []
            ]

        -- ##### We partner with #####
        , column [ wf, rsPortion.bg, hf, paddingXY 28 100, spacingXY 0 24, centerX, hf ]
            [ paragraph [ Font.center, Font.size 28, Font.color colors.primary, centerY ] [ text "We partner with the most trusted names in the business." ]
            , paragraph [ centerY, centerX, Font.center, width (fill |> Element.maximum 600), lineHeight 1.6 ] [ text "Flint's industry partnerships mean the highest standards in nurse quality and competency." ]
            ]
        ]



----- #### JoinTeam #### -------


joinTeamView : Device.Device -> Element msg
joinTeamView device =
    column
        [ Background.color colors.cremeDark
        , wf
        , hf
        , Font.family [ Font.typeface "Inter" ]
        ]
        [ row [ wf ]
            [ row [ width <| fillPortion 2 ] [ Element.none ]
            , column [ width <| fillPortion 8 ] [ joinTeamBody device ]
            , row [ width <| fillPortion 2 ] [ Element.none ]
            ]
        ]


joinTeamBody : Device.Device -> Element msg
joinTeamBody device =
    let
        titleStyle =
            [ Font.center
            , Font.size 28
            , Font.semiBold
            , Font.color colors.primary
            ]

        interviewProcessSm =
            row [ centerX ]
                [ Element.image [ css "max-width" "100%", css "height" "auto" ] { src = "/static/images/interview-process-sm.png", description = "Flint interview process" }
                ]
    in
    column [ wf, centerX, paddingXY 0 48, spacingXY 0 56, Font.size 16 ]
        [ column [ centerX ]
            [ paragraph titleStyle
                [ text "We work with the very best" ]
            ]
        , wrappedRow [ alignTop, spacingXY 24 20 ]
            [ paragraph [ lineHeight 1.6, minW 300 ]
                [ text "At Flint, we're committed to hiring the best people to build our teams. Building great products takes smart, disciplined, and empathetic individuals who understand our product goals and imagine innovative ways to achieve results. We designed a hiring process to help us identify those people." ]
            , paragraph
                [ wf
                , hf
                , lineHeight 1.6
                , minW 300
                ]
                [ text "Flint fosters a culture of respect, dialogue, and growth– a home where our team members can engage in a continuous conversation about product, engineering, and learning. "
                , Element.link [ Font.underline ]
                    { url = toPath (Blog "culture")
                    , label = el [ lineHeight 1.6 ] (text "Read more about our values and culture.")
                    }
                , paragraph
                    [ lineHeight 1.6
                    ]
                    [ text " We interview and make hires within a week from our first meet – it's a commitment." ]
                ]
            ]
        , case device of
            Device.Phone _ ->
                interviewProcessSm

            Device.Tablet _ ->
                interviewProcessSm

            _ ->
                row [ centerX ]
                    [ Element.image [ css "max-width" "100%", css "height" "auto" ] { src = "/static/images/interview-process.png", description = "Flint interview process" }
                    ]
        ]



----- #### HEADER #### -------


toHeader : Device.Device -> Model -> Element Msg
toHeader device model =
    case model.config.page_ of
        NurseCareersPage ->
            nurseCareerHeader device model

        JoinTheTeamPage ->
            joinTeamHeader device model


joinTeamHeader : Device.Device -> Model -> Element Msg
joinTeamHeader device model =
    let
        bg =
            [ css "background" "#FFDCC9"
            , css "background" "linear-gradient(281.5deg, #FFDCC9 -0.43%, #C8BCC7 8.22%, #8284AF 27.81%, #6E74A9 52.4%, #6359A1 82.46%)"
            ]

        menu =
            [ ( "Partnerships", Partnerships ), ( "Nurse Careers", NurseCareers "" ) ]

        blobSrc =
            "/static/images/header-blob-blue.svg"

        title =
            "Join the Team"
    in
    header device { title = title, menu = menu, bg = bg, blobSrc = blobSrc } model


nurseCareerHeader : Device.Device -> Model -> Element Msg
nurseCareerHeader device model =
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
            "Your success is Flint's success"
    in
    header device { title = title, menu = menu, bg = bg, blobSrc = blobSrc } model


header :
    Device.Device
    ->
        { title : String
        , menu : List ( String, Page )
        , bg : List (Attribute Msg)
        , blobSrc : String -- url
        }
    -> Model
    -> Element Msg
header device { title, menu, bg, blobSrc } model =
    let
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
                                , Font.color colors.white
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
                [ el ([ wf, centerX, Font.size rs.titleFontSize ] ++ Heading.h1 ++ Styles.title)
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


type alias Viewer =
    { jobView : ( String, String, Job ) -> Element Msg
    , applyView : Job -> Model -> Element Msg
    , copyView : Config -> Element Msg
    }


desktopView : Viewer
desktopView =
    { jobView = desktopJobView
    , applyView = desktopApplyView
    , copyView = desktopCopyView
    }


phoneView : Viewer
phoneView =
    { jobView = phoneJobView
    , applyView = phoneApplyView
    , copyView = phoneCopyView
    }


jobsView : Device.Device -> Viewer -> Model -> Element Msg
jobsView device viewer model =
    let
        rsPadding =
            -- responsive padding
            case device of
                Device.Phone _ ->
                    paddingXY 20 40

                _ ->
                    paddingXY 100 40

        openJobsHeader =
            case model.config.page_ of
                NurseCareersPage ->
                    column [ wf, spacingXY 10 24, Styles.pb 24 ]
                        [ paragraph
                            [ Font.size 26
                            , Font.semiBold
                            , Font.color colors.primary
                            , Font.center
                            ]
                            [ text "Open Positions" ]
                        , column [ width (fill |> Element.maximum 820), centerX ]
                            [ paragraph
                                [ Font.center
                                , lineHeight 1.6
                                , pt 12
                                , Font.justify
                                ]
                                [ text "Apply now and discover what exciting new career opportunities with growth potential awaits you in America, where you will apply your existing skills and knowledge while learning new ones. Our team of experienced nurse educators will guide you and start your journey today."
                                ]
                            ]
                        ]

                JoinTheTeamPage ->
                    paragraph
                        [ Font.size 24
                        , Styles.headFont
                        , Font.color colors.primary
                        , Font.center
                        , Styles.pb 24
                        ]
                        [ text "Open Positions"
                        ]
    in
    case model.view of
        JobsView ->
            case model.jobs of
                NotAsked ->
                    column [ wf, minH 120 ]
                        [ row [ centerX, centerY ] [ paragraph [ Font.center ] [ text "Loading jobs" ] ]
                        ]

                Loading ->
                    column [ wf, minH 120 ]
                        [ row [ centerX, centerY ] [ paragraph [ Font.center ] [ text "Loading jobs" ] ]
                        ]

                Failure err ->
                    column [ wf, minH 120 ]
                        [ row [ centerX, centerY ] [ paragraph [ Font.center ] [ text "An error occured trying to load jobs" ] ]
                        ]

                Success jobs ->
                    column
                        [ hf
                        , centerX
                        , width <| maximum 1500 fill
                        ]
                        [ column
                            [ spacingXY 0 20
                            , rsPadding
                            , wf
                            , centerX
                            ]
                          <|
                            [ viewer.copyView model.config
                            , column [ wf, spacing 40, paddingXY 0 40 ]
                                (openJobsHeader
                                    :: (Dict.toList jobs |> List.map (\( id, job ) -> ( model.config.page, id, job )) |> List.map viewer.jobView)
                                )
                            ]
                        ]

        ApplyView jobId ->
            let
                jobsD =
                    case model.jobs of
                        NotAsked ->
                            Dict.empty

                        Loading ->
                            Dict.empty

                        Failure err ->
                            Dict.empty

                        Success jobs ->
                            jobs
            in
            case Dict.get jobId jobsD of
                Just job ->
                    column [ centerX, spacingXY 0 100, wf, centerX ]
                        [ column [ width <| maximum 800 fill, centerX ]
                            [ paragraph Styles.heading
                                [ text job.title
                                ]
                            , column (Styles.paragraph ++ [ spacing 20 ])
                                (case job.description of
                                    "" ->
                                        [ none ]

                                    desc ->
                                        Mark.default desc
                                )
                            ]
                        , viewer.applyView job model
                        ]

                Nothing ->
                    jobsView device viewer { model | view = JobsView }


desktopJobView : ( String, String, Job ) -> Element Msg
desktopJobView ( page, id, job ) =
    row [ wf ]
        [ column [ alignLeft, spacingXY 0 10, wf ]
            [ link
                [ Font.color colors.primary
                , mouseOver
                    [ Font.color colors.carminePink
                    ]
                ]
                { url = absolute [ page, id ] []
                , label = paragraph [ wf, spacing 10 ] [ text job.title ]
                }
            , wrappedRow [ spacingXY 10 10, Font.size 15, wf ]
                [ text job.location
                , text job.equity
                , text job.experience
                ]
            ]
        , column [ height fill, alignTop, alignRight ]
            [ Input.button Styles.btnOutline
                { onPress = Just (Apply True id)
                , label = text "Apply Now"
                }
            ]
        ]


phoneJobView : ( String, String, Job ) -> Element Msg
phoneJobView ( page, id, job ) =
    row [ wf, spacing 5 ]
        [ column [ alignLeft, spacingXY 0 10, wf ]
            [ paragraph [ wf ]
                [ link
                    [ Font.color colors.primary
                    , mouseOver
                        [ Font.color colors.carminePink
                        ]
                    ]
                    { url = absolute [ page, id ] []
                    , label = paragraph [ wf, spacing 10 ] [ text job.title ]
                    }
                ]
            , wrappedRow [ spacingXY 10 10, Font.size 15, wf ]
                [ text job.location
                , text job.equity
                , text job.experience
                ]
            ]
        , column [ alignRight ]
            [ Input.button Styles.btnOutline
                { onPress = Just (Apply True id)
                , label = text "Apply Now"
                }
            ]
        ]


textbox : List (Attribute msg)
textbox =
    [ Border.width 1
    , padding 7
    , focused [ Border.color colors.primary ]
    ]


multitextbox : List (Attribute msg)
multitextbox =
    [ Border.width 1
    , padding 7
    , height (px 100)
    , focused [ Border.color colors.primary ]
    ]


textboxLabel : List (Attribute msg)
textboxLabel =
    [ Font.size 15
    , alignTop
    , wf
    ]


smallHeading : List (Attribute msg)
smallHeading =
    [ Font.size 24
    , Styles.font
    ]


desktopApplyView : Job -> Model -> Element Msg
desktopApplyView job model =
    column [ centerX, spacing 10, width <| minimum 300 <| maximum 500 fill ] <|
        [ el smallHeading <| text "Apply"
        , Input.username textbox
            { onChange = Set FirstName
            , text = Text.toString model.applicant.firstName
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "First Name"
            }
        , Input.username textbox
            { onChange = Set LastName
            , text = Text.toString model.applicant.lastName
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "Last Name"
            }
        , Input.email textbox
            { onChange = Set Email
            , text = Text.toString model.applicant.email
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "Email"
            }
        , Input.text textbox
            { onChange = Set Phone
            , text = Text.toString model.applicant.phone
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "Phone"
            }
        , Input.button (Font.size 15 :: Styles.btnOutline)
            { onPress = Just UploadResume
            , label = text "Upload Resume"
            }
        , case model.applicant.resume of
            Just file ->
                el [ Font.size 15 ] <| text <| File.name file

            Nothing ->
                none
        , Input.multiline multitextbox
            { onChange = Set Reason
            , text = Text.toString model.applicant.reason
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text model.config.copy.why
            , spellcheck = True
            }
        , case model.error of
            Just err ->
                paragraph [ Font.size 15 ]
                    [ text err
                    ]

            Nothing ->
                case model.success of
                    Just msg ->
                        paragraph [ Font.size 15 ]
                            [ text msg
                            ]

                    Nothing ->
                        none
        ]
            ++ (case model.success of
                    Just _ ->
                        []

                    Nothing ->
                        [ Input.button (Font.size 15 :: Styles.btnOutline)
                            { onPress = Just (Submit job)
                            , label = text "Submit"
                            }
                        ]
               )


phoneApplyView : Job -> Model -> Element Msg
phoneApplyView job model =
    column [ centerX, spacing 10, wf ] <|
        [ el smallHeading <| text "Apply"
        , Input.username textbox
            { onChange = Set FirstName
            , text = Text.toString model.applicant.firstName
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "First Name"
            }
        , Input.username textbox
            { onChange = Set LastName
            , text = Text.toString model.applicant.lastName
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "Last Name"
            }
        , Input.email textbox
            { onChange = Set Email
            , text = Text.toString model.applicant.email
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "Email"
            }
        , Input.text textbox
            { onChange = Set Phone
            , text = Text.toString model.applicant.phone
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "Phone"
            }
        , Input.button (Font.size 15 :: Styles.btnOutline)
            { onPress = Just UploadResume
            , label = text "Upload Resume"
            }
        , case model.applicant.resume of
            Just file ->
                el [ Font.size 15 ] <| text <| File.name file

            Nothing ->
                none
        , Input.multiline multitextbox
            { onChange = Set Reason
            , text = Text.toString model.applicant.reason
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| paragraph [] [ text model.config.copy.why ]
            , spellcheck = True
            }
        , case model.error of
            Just err ->
                paragraph [ Font.size 15 ]
                    [ text err
                    ]

            Nothing ->
                case model.success of
                    Just msg ->
                        paragraph [ Font.size 15 ]
                            [ text msg
                            ]

                    Nothing ->
                        none
        ]
            ++ (case model.success of
                    Just _ ->
                        []

                    Nothing ->
                        [ Input.button (Font.size 15 :: Styles.btnOutline)
                            { onPress = Just (Submit job)
                            , label = text "Submit"
                            }
                        ]
               )


desktopCopyView : Config -> Element Msg
desktopCopyView config =
    Element.none


phoneCopyView : Config -> Element Msg
phoneCopyView config =
    Element.none


wf : Attribute msg
wf =
    width fill
