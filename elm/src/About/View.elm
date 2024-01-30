module About.View exposing (view)

import About.Types exposing (Model, Msg(..))
import Device exposing (Device)
import Element
    exposing
        ( Element
        , alignTop
        , centerX
        , centerY
        , column
        , fill
        , height
        , htmlAttribute
        , maximum
        , newTabLink
        , none
        , padding
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
import Html.Attributes
import Layout exposing (Layout, footer)
import Router.Routes exposing (Page(..))
import Styles exposing (colors, css, hf, lineHeight, maxW, minW, pb, pt, wf)


type alias Profile =
    { name : String
    , position : String
    , bio : Maybe String
    , linkedin : Maybe String
    , url : String -- ideal pic 408x397
    }


type Card
    = Member Profile
    | Blank


view : { device : Device, model : Model, showNavMenu : Bool } -> Layout Msg
view { device, showNavMenu } =
    { phone =
        [ column
            [ wf
            , Font.family [ Font.typeface "Inter" ]
            , Background.color colors.cremeDark
            , css "position" "relative"
            ]
            [ row [ wf, hf, css "position" "relative" ] [ header device showNavMenu ]
            , column [ wf, hf, paddingXY 48 0 ] [ body ]
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
            , column [ wf, hf, paddingXY 48 0 ] [ body ]
            , column [ wf, pt 120 ] footer.phone
            ]
        ]
    , desktop =
        [ column
            [ wf
            , Font.family [ Font.typeface "Inter" ]
            , css "position" "relative"
            , Background.color colors.cremeLight
            ]
            [ row [ wf, hf ] [ header device showNavMenu ]
            , column [ wf, hf, paddingXY 200 0, Background.color colors.cremeDark ] [ body ]
            , column [ wf ] footer.desktop
            ]
        ]
    }


body : Element Msg
body =
    column
        [ width (fill |> maximum 1400)
        , centerX
        ]
        [ whoWeAre
        , team
        ]


whoWeAre : Element msg
whoWeAre =
    let
        titleStyle =
            [ Font.center
            , Font.size 28
            , Font.semiBold
            , Font.color colors.primary
            ]
    in
    column [ wf, centerX, paddingXY 0 48, spacingXY 0 56, Font.size 16 ]
        [ column [ centerX ]
            [ paragraph titleStyle
                [ text "Shaping the Future of Nursing" ]
            ]
        , wrappedRow [ alignTop, spacingXY 24 20 ]
            [ aboutFlint
            , aboutTeam
            ]
        ]


aboutFlint : Element msg
aboutFlint =
    copy.right
        |> List.map
            (text
                >> List.singleton
                >> paragraph [ lineHeight 1.6, minW 300 ]
            )
        |> column [ alignTop, wf, spacingXY 0 12 ]


aboutTeam : Element msg
aboutTeam =
    copy.left
        |> List.map
            (text
                >> List.singleton
                >> paragraph [ lineHeight 1.6, minW 300 ]
            )
        |> column [ alignTop, wf, spacingXY 0 12 ]


team : Element Msg
team =
    let
        titleStyle =
            [ Font.center
            , Font.size 28
            , Font.semiBold
            , Font.color colors.primary
            ]
    in
    column
        [ wf
        , centerX
        , paddingXY 0 48
        , pb 120
        ]
        [ row [ centerX ]
            [ paragraph titleStyle
                [ text "Meet our Team" ]
            ]
        , wrappedRow [ wf, hf, pt 80, spaceEvenly, spacing 20 ] (list |> List.map card)
        ]


list : List Card
list =
    [ Member
        { name = "Kenton Jarvie"
        , position = "CEO"
        , bio = Just "That's the guy who opens the meetings. No one really knows what he's doing, really. Mostly planning his next surf retreat it seems."
        , linkedin = Just "https://www.linkedin.com/in/kentonjarvie/"
        , url = "static/headshots/kenton.png"
        }
    , Member
        { name = "Marianne Cabalida"
        , position = "Executive Assistant"
        , bio = Just "With her deep-rooted expertise in administration and organizational dynamics, Marianne is the backbone of Flint's day-to-day accounting and people operations."
        , linkedin = Just "https://www.linkedin.com/in/mariannejcab/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Montse del Toro"
        , position = "Senior Nurse Success Manager"
        , bio = Just "With unwavering passion, Montse oversees Nurse Success and product operations, ensuring steadfast support to the nurses at every stage of their journey."
        , linkedin = Just "https://www.linkedin.com/in/montserrat-del-toro/"
        , url = "static/headshots/montse-sm.jpg"
        }
    , Member
        { name = "Samuel Adedayo"
        , position = "Nurse Success Advisor"
        , bio = Just "An anchor of nurse success within Flint, Samuel dispenses insights and guidance, propelling nurses towards unparalleled professional journey."
        , linkedin = Just "https://www.linkedin.com/in/samuel-adedayo-62b479145/"
        , url = "static/headshots/samuel-sm.jpg"
        }
    , Member
        { name = "Chelsea Mansour"
        , position = "Nurse Success Advisor"
        , bio = Just "Chelsea's unwavering dedication reflects in her personalized guidance, fortifying each nurse's unique trajectory within Flint."
        , linkedin = Just "https://www.linkedin.com/in/chelsea-mansour-184600206/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Anson Kung"
        , position = "COO"
        , bio = Just "Anson relentlessly champions quality of sleep by giving the most hollow speeches. When he starts talking, it's nap time: just nod and pretend to be interested, he's mostly talking to hilmself anyway."
        , linkedin = Just "https://www.linkedin.com/in/ansonkung/"
        , url = "static/headshots/anson-sm.jpg"
        }
    , Member
        { name = "Neil Prigge"
        , position = "Head of Partnerships"
        , bio = Just "Neil is the live embodiment of Flint's spirit: he will stab you in the back, but he'll do it with a smile."
        , linkedin = Just "https://www.linkedin.com/in/neil-prigge/"
        , url = "static/headshots/neil-sm.jpg"
        }
    , Member
        { name = "Rovina D'Souza"
        , position = "Chief of Staff"
        , bio = Just "Operating as a Swiss army knife,  heading strategic projects within Flint, Rovina is dedicated to ensuring all strategic initiatives are executed with precision and efficiency."
        , linkedin = Just "https://www.linkedin.com/in/rovinadsouza/"
        , url = "static/headshots/rovina.jpg"
        }
    , Blank
    , Blank
    , Blank
    ]


card : Card -> Element msg
card p =
    case p of
        Member profile ->
            let
                position =
                    paragraph [ Font.size 16, Font.semiBold ] [ text profile.position ]

                name =
                    paragraph [ Font.size 18, Font.semiBold, Font.color colors.black ] [ text profile.name ]

                linkedin =
                    case profile.linkedin of
                        Just link ->
                            newTabLink
                                []
                                { url = link
                                , label =
                                    Element.image
                                        [ centerY
                                        , Font.alignLeft
                                        , width (px 25)
                                        , height (px 25)
                                        ]
                                        { src = "/static/images/linkedin-icon-2.svg?new"
                                        , description = String.concat <| [ "Linkedin profile link for ", profile.name ]
                                        }
                                }

                        Nothing ->
                            none
            in
            row
                [ wf
                , hf
                ]
                [ column
                    [ wf
                    , hf
                    , Background.color colors.white
                    , Border.rounded 8
                    , centerX
                    ]
                    [ row [ wf ]
                        [ Element.image [ css "width" "100%", maxW 408, minW 298 ]
                            { src = profile.url
                            , description = String.concat <| [ "Headshot picture of ", profile.name ]
                            }
                        ]
                    , column [ paddingXY 16 16, spacingXY 0 12, wf ]
                        [ row [ wf, spaceEvenly ] [ name, linkedin ]
                        , position
                        ]
                    , case profile.bio of
                        Just bio ->
                            paragraph [ wf, padding 16, Font.size 14 ] [ text bio ]

                        Nothing ->
                            none
                    ]
                ]

        Blank ->
            row
                [ wf
                , hf
                ]
                [ row [ wf ]
                    [ Element.el [ css "width" "100%", maxW 408, minW 298 ] (text "")
                    ]
                ]


header : Device.Device -> Bool -> Element Msg
header device showNavMenu =
    Layout.header
        { device = device
        , title = "Freedom. Equality. Quality."
        , navigations =
            [ ( Partnerships, "Partnerships" )
            , ( NurseCareers "", "Nurse Careers" )
            , ( Blog "", "Blog" )
            , ( About, "About" )
            ]
        , attributes =
            [ htmlAttribute <| Html.Attributes.style "position" "relative"
            , htmlAttribute <| Html.Attributes.style "background" "linear-gradient(281.5deg, #FFDCC9 -0.43%, #C8BCC7 8.22%, #8284AF 27.81%, #6E74A9 52.4%, #6359A1 82.46%)"
            ]
        , headerIconBg = Layout.HeaderIconBgBlue
        , showMenu = showNavMenu
        , toggleNavMenuMsg = ToggleNavMenu
        }


copy : { right : List String, left : List String }
copy =
    { right = [ "Flint is driven by a bold mission to revolutionize American healthcare by addressing its most pressing challenge: the shortage of nurses. Our purpose is to dismantle the obstacles that hinder international nurses from embarking on a transformative journey to establish their careers in the United States. Our unwavering commitment aims to assist more than a thousand nurses in making this transition by the culmination of the upcoming year." ]
    , left = [ "Our geographically diverse team operates remotely, spanning multiple nations, and is comprised of seasoned professionals hailing from diverse sectors like technology, healthcare, and immigration. At Flint, our focus transcends the conventional. We are resolute in reshaping the landscape of nursing, envisioning a future where barriers are dismantled, opportunities are boundless, and the foundation of healthcare is fortified by a thriving community of nurses from around the world." ]
    }
