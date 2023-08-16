module Partnerships.View exposing (view)

import Device exposing (Device)
import Element
    exposing
        ( Element
        , centerX
        , column
        , fill
        , fillPortion
        , height
        , htmlAttribute
        , padding
        , paddingXY
        , paragraph
        , px
        , rgb255
        , row
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Layout exposing (Layout, footer)
import Partnerships.Types exposing (Model, Msg(..))
import Router.Routes exposing (Page(..))
import Styles exposing (colors, css, hf, lineHeight, maxW, paddingE, wf)


view : { device : Device, model : Model, showNavMenu : Bool } -> Layout Msg
view props =
    { phone =
        [ column
            [ wf
            , height fill
            ]
            (desktopView props
                ++ footer.phone
            )
        ]
    , tablet =
        [ column
            [ wf
            ]
            (desktopView props
                ++ footer.tablet
            )
        ]
    , desktop =
        [ column
            [ wf
            ]
            (desktopView props
                ++ footer.desktop
            )
        ]
    }


setResponsiveVal : Device.Device -> { phone : a, tablet : a, desktop : a, notSet : a } -> a
setResponsiveVal device { phone, desktop, tablet, notSet } =
    case device of
        Device.Phone _ ->
            phone

        Device.Desktop _ ->
            desktop

        Device.Tablet _ ->
            tablet

        Device.NotSet ->
            notSet


desktopView : { device : Device, model : Model, showNavMenu : Bool } -> List (Element Msg)
desktopView { device, showNavMenu } =
    let
        fillPortionVal =
            setResponsiveVal device { phone = 0, desktop = 2, tablet = 2, notSet = 0 }
    in
    [ column
        [ Background.color colors.cremeDark
        , wf
        , hf
        , Font.family [ Font.typeface "Inter" ]
        ]
        [ header device showNavMenu
        , row
            [ wf ]
            [ row [ width <| fillPortion fillPortionVal ] [ Element.none ]
            , column [ width <| fillPortion 8 ] [ section0 device ]
            , row [ width <| fillPortion fillPortionVal ] [ Element.none ]
            ]
        , Layout.partners device
        ]
    ]


header : Device.Device -> Bool -> Element Msg
header device showNavMenu =
    Layout.header
        { device = device
        , title = "Recreate the way you hire nurses"
        , navigations =
            [ ( Partnerships, "Partnerships" )
            , ( NurseCareers "", "Nurse Careers" )
            , ( Blog "", "Blog" )
            , ( About, "About" )
            ]
        , attributes =
            [ htmlAttribute <| Html.Attributes.style "position" "relative"
            , htmlAttribute <| Html.Attributes.style "background" "linear-gradient(281.17deg, #A7C8F9 -8.91%, #8494C7 12.48%, #6E74A9 42.43%, #626297 82.36%)"
            ]
        , headerIconBg = Layout.HeaderIconBgBlue
        , showMenu = showNavMenu
        , toggleNavMenuMsg = ToggleNavMenu
        }


section0 : Device.Device -> Element msg
section0 device =
    let
        titleStyle =
            [ Font.center
            , Font.size 28
            , Font.semiBold
            , Font.color colors.primary
            ]
    in
    column [ wf, centerX, paddingXY 0 48, spacingXY 0 48 ]
        [ column [ spacingXY 0 12, centerX ]
            [ paragraph titleStyle
                [ text "America is short on nurses." ]
            , paragraph titleStyle
                [ text "Flint brings top international nurses into healthcare facilities nationwide." ]
            ]
        , paragraph
            [ paddingE 12 18 0 18, Font.center, lineHeight 1.6, centerX ]
            [ text "Hiring internationally is complicated and risky. Flint makes it simple and predictable. By sourcing in multiple countries, we can service your facility's needs. Our technology enables us to overcome immigration and hiring variables that others cannot, which means a fast turnaround." ]
        , column
            [ centerX, spacingXY 0 24 ]
            [ valueCard device experiencedNurses
            , valueCard device savings
            , valueCard device neverBeShortNurses
            ]
        ]


experiencedNurses : { iconUrl : String, iconDesc : String, heading : String, desc : String }
experiencedNurses =
    { iconUrl = "/static/images/partnerships-nurse.svg"
    , iconDesc = "Flint - Experienced Nurses"
    , heading = "Recruit experienced nurses who have committed for 3+ years in your position"
    , desc = "Our nurses have years of clinical experience and are looking to build a long-term career at the right facility."
    }


savings : { iconUrl : String, iconDesc : String, heading : String, desc : String }
savings =
    { iconUrl = "/static/images/partnerships-savings.svg"
    , iconDesc = "Flint - Save costs by partnerting with Flint"
    , heading = "Save millions by replacing temporary staff with your own employees"
    , desc = "On average, we help facilities save 50% in staffing costs compared with agencies. For every ten nurses sourced through Flint, expect to save over $1M annually."
    }


neverBeShortNurses : { iconUrl : String, iconDesc : String, heading : String, desc : String }
neverBeShortNurses =
    { iconUrl = "static/images/partnerships-never-short-nurses.svg"
    , iconDesc = "Flint - Never be short on nurses again"
    , heading = "Never be short on nurses again"
    , desc = "It's not just about today; it's about tomorrow. We work with you to develop a sustainable recruiting pipeline that you can count on for years to come."
    }


valueCard : Device.Device -> { iconUrl : String, iconDesc : String, heading : String, desc : String } -> Element msg
valueCard device { iconUrl, iconDesc, heading, desc } =
    let
        responsiveDiv =
            case device of
                Device.Phone _ ->
                    column

                Device.Tablet _ ->
                    row

                Device.Desktop _ ->
                    row

                Device.NotSet ->
                    row
    in
    responsiveDiv [ spacingXY 12 12, Background.color colors.cremeLight, Border.rounded 12, padding 24 ]
        [ row [ centerX ]
            [ Element.image [ centerX, width (px 72), height (px 87) ] { src = iconUrl, description = iconDesc }
            ]
        , column [ maxW 550, spacingXY 0 12, padding 12 ]
            [ paragraph [ css "width" "100%", Font.color colors.primary, Font.bold ] [ text heading ]
            , paragraph [ css "width" "100%", Font.color (rgb255 25 21 41) ] [ text desc ]
            ]
        ]
