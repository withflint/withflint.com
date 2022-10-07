module Australia.View exposing (view)

import Apply exposing (Field(..), Job)
import Australia.Types exposing (Model, Msg(..))
import Device
import Element
    exposing
        ( Attribute
        , Element
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
        , maximum
        , minimum
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
import Layout exposing (Layout, footer, menu, phoneMenu, topMenu)
import Mark
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, css, hf, lineHeight, minW, palette, pt, wf, wp)
import Text


copy : { why : String, applyNow : String, left : String, right : String, offer : String }
copy =
    { why = "Why do you want to work in America?"
    , applyNow = "Apply Now"
    , left = "Flint offers expertise in helping **Australian Registered Nurses** obtain a license, a work permit and a profitable job offer directly from the facilities we partner with in the USA. A seamless transition without the hassle."
    , right = "We help our nurses with a personal touch as we understand that each nurse is unique, we walk alongside our nurses in their development during our process, and we assist with relocation and orientation towards the new job and their new home. Flint is here to partner with you."
    , offer = """
**You must be willing to relocate to USA. Flint covers the relocation.**

## Job Description
A Surgical/Operating Nurse uses the nursing process, to plan, evaluate, and deliver patient care that meets the identified needs of patients having operative and procedural interventions. Nurses applying to this position must have experience caring for patients in the inpatient or ambulatory surgical setting.


## Responsibilities
- Demonstrates clinical competence in providing direct patient care in the pre, intra, and post-operative care settings.
- Able to assess and adapt quickly to a change in patient status by choosing appropriate nursing interventions based on the situation, and American Heart Association guidelines (ACLS, PALS, BLS).
- Follow and demonstrate competency of the Joint Commissions (JCAHO) recommendations for Patient Safety in the Surgical Setting.
- Collaborate with physicians and nurses to plan and provide care to patients and revise the plan of care to reflect changing patient needs based on evaluation of the patient’s status.
- Assures the appropriate orders and pre-operative diagnostic testing, and physical exam notes are completed and present.
- Collaborate with multiple disciplines/departments to improve delivery of patient care and provide a safe environment for patients undergoing surgical and procedural intervention.
- Assist surgeons during operations and/or physicians in various procedures.
- Monitor and assess patients before, during, and post-operations and procedures.
- Admit, treat, and discharge patients in the ambulatory care setting, or admit/transfer patients to an inpatient setting.
- Provides pre and post-operative instructions and education to patients.

## Requirements
- A registered nurse (RN) license with NCLEX
- A “4 year bachelor” degree in nursing.
- Willing to relocate to the US.
- Australian passport.

## Why Join the Flint Nurse Network
If you want to find your dream job in the USA but never knew how or where to start? Flint might be just what you’re looking for. We offer relocation assistance and direct hire opportunities . This means no agency salary retention, and you get to choose where you interview and work. As a Flint nurse you’ll have new opportunities to grow professionally, gain hands-on experience as a nurse in the USA, and have ample opportunity to discover new people and places.
Apply to find out all the reasons you should consider becoming a Flint nurse.
"""
    }


job : { url : String, title : String, location : String, equity : String, experience : String, description : String }
job =
    { url = ""
    , title = "Registered Nurse for Australians"
    , location = "USA"
    , equity = "0"
    , experience = "5 years+"
    , description = "a good job"
    }


view : Device.Device -> Model -> Layout Msg
view device model =
    let
        render view__ =
            if model.isPhoneMenuVisible then
                column [ wf, hf, css "position" "relative" ] [ phoneMenu PhoneMenuToggle model.isPhoneMenuVisible ]
                    |> List.singleton

            else
                view__
    in
    { phone =
        render <|
            [ column
                [ wf
                , Font.family [ Font.typeface "Inter" ]
                ]
                [ row [ wf, hf ] [ header_ device model ]
                , view_ device model
                ]
            , column [ wf ] footer.phone
            ]
    , desktop =
        render <|
            [ column
                [ wf
                , Font.family [ Font.typeface "Inter" ]
                ]
                [ row [ wf, hf ] [ header_ device model ]
                , view_ device model
                ]
            , column [ wf ] footer.desktop
            ]
    , tablet =
        render <|
            [ column
                [ wf
                , Font.family [ Font.typeface "Inter" ]
                ]
                [ row [ wf, hf ] [ header_ device model ]
                , view_ device model
                ]
            , column [ wf ] footer.phone
            ]
    }


view_ : Device.Device -> Model -> Element Msg
view_ device model =
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
            [ row [ width <| fillPortion 1 ] [ Element.none ]
            , column [ width <| fillPortion 8 ] [ body device model ]
            , row [ width <| fillPortion 1 ] [ Element.none ]
            ]
        , states device
        , partners device
        ]


body : Device.Device -> Model -> Element Msg
body device model =
    let
        titleStyle =
            [ Font.center
            , Font.size 28
            , Font.semiBold
            , Font.color colors.primary
            ]

        rsJustify =
            case device of
                Device.Phone _ ->
                    Font.center

                _ ->
                    Font.justify

        rsDiv =
            case device of
                Device.Phone _ ->
                    column

                _ ->
                    row
    in
    column [ wf, centerX, paddingXY 0 48, spacingXY 0 48 ]
        [ column [ centerX ]
            [ paragraph titleStyle
                [ text "Registered Nurse" ]
            ]
        , rsDiv [ spacingXY 34 0, alignTop, spacingXY 40 48 ]
            [ paragraph [ alignTop, Font.center, pt 12, rsJustify, lineHeight 1.6 ] <|
                Mark.default copy.left
            , paragraph
                [ Font.center
                , alignTop
                , pt 12
                , rsJustify
                , lineHeight 1.6
                ]
              <|
                Mark.default copy.right
            ]
        , column [ wf, spacingXY 0 44, pt 24 ]
            [ advantages device
            , info
            , jobsView device model
            ]
        ]


advantages : Device.Device -> Element msg
advantages _ =
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


info : Element msg
info =
    column [ wf, paddingEach { top = 64, bottom = 48, right = 0, left = 0 }, spacingXY 96 40 ] <|
        Mark.default copy.offer


states : Device.Device -> Element msg
states device =
    let
        titleStyle =
            [ Font.center
            , Font.size 24
            , Font.semiBold
            , Font.color colors.primary
            , padding 10
            ]

        space =
            case device of
                Device.Phone _ ->
                    spacing 20

                _ ->
                    spacing 100
    in
    row [ wf, paddingXY 12 56 ]
        [ column [ wf, spacingXY 0 48 ]
            [ paragraph titleStyle [ text "US states where you can live and work" ]
            , wrappedRow
                [ wf
                , Font.color colors.primary
                , centerX
                , spaceEvenly
                , space
                ]
                [ column [ centerX, spacingXY 0 22, alignTop ]
                    [ paragraph [ Font.center ] [ text "Colorado" ]
                    , paragraph [ Font.center ] [ text "Missouri" ]
                    , paragraph [ Font.center ] [ text "North Carolina" ]
                    ]
                , column [ centerX, spacingXY 0 22, alignTop ]
                    [ paragraph [ Font.center ] [ text "Tennessee" ]
                    , paragraph [ Font.center ] [ text "Washington" ]
                    , paragraph [ Font.center ] [ text "Wisconsin" ]
                    ]
                ]
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
        , column [ wf, rsPortion.bg, hf, paddingXY 28 100, spacingXY 0 24, centerX, hf ]
            [ paragraph [ Font.center, Font.size 28, Font.color colors.primary, centerY ] [ text "We partner with the most trusted names in the business." ]
            , paragraph [ centerY, centerX, Font.center, width (fill |> Element.maximum 600), lineHeight 1.6 ] [ text "Flint's industry partnerships mean the highest standards in nurse quality and competency." ]
            ]
        ]


header_ : Device.Device -> Model -> Element Msg
header_ device model =
    let
        bg =
            [ css "background" "rgb(68,55,109)"
            , css "background" "linear-gradient(282.96deg, #E54848 -0.52%, #BA4352 8.17%, #7E3D60 37.38%, #5D3968 66.24%)"
            ]

        blobSrc =
            "/static/images/header-blob-beige.svg"

        title =
            "Want to be a nurse in the USA?"
    in
    header device { title = title, menu = topMenu, bg = bg, blobSrc = blobSrc } model


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
            [ case device of
                Device.Phone _ ->
                    row [ wf, height <| fillPortion 4 ] [ Element.none ]

                _ ->
                    row [ wf, height <| fillPortion 4 ]
                        [ row [ wf ]
                            [ row [ width <| fillPortion 7 ] []
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
                            , row [ width <| fillPortion 2 ] []
                            ]
                        ]
            , row [ wf, height <| fillPortion 8, padding 10 ]
                [ el ([ wf, centerX, Font.size rs.titleFontSize ] ++ Heading.h1 ++ Styles.title)
                    (paragraph [ Font.center, Font.size rs.titleFontSize ] [ text title ])
                ]
            , case device of
                Device.Phone _ ->
                    Element.none

                _ ->
                    row [ wf, height <| fillPortion 2 ] []
            ]
        ]


jobsView : Device.Device -> Model -> Element Msg
jobsView device model =
    column
        [ hf
        , centerX
        , width <| maximum 1500 fill
        ]
        [ column
            [ spacingXY 0 20
            , wf
            , centerX
            ]
            [ column [ spacing 40, paddingXY 0 40, width (fill |> Element.maximum 1000), centerX ]
                [ case device of
                    Device.Phone _ ->
                        phoneApplyView job model

                    _ ->
                        desktopApplyView job model
                ]
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
    , paddingXY 0 20
    ]


desktopApplyView : Job -> Model -> Element Msg
desktopApplyView job_ model =
    column [ centerX, spacing 20, width <| minimum 300 <| maximum 500 fill ] <|
        [ el smallHeading <| text copy.applyNow
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
            , label = Input.labelAbove textboxLabel <| text copy.why
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
                            { onPress = Just (Submit job_)
                            , label = text "Submit"
                            }
                        ]
               )


phoneApplyView : Job -> Model -> Element Msg
phoneApplyView job_ model =
    column [ centerX, spacing 10, wf ] <|
        [ el smallHeading <| text copy.applyNow
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
            , label = Input.labelAbove textboxLabel <| paragraph [] [ text copy.why ]
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
                            { onPress = Just (Submit job_)
                            , label = text "Submit"
                            }
                        ]
               )
