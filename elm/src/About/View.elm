module About.View exposing (view)

import About.Types exposing (Model, Msg(..))
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
import Html
import Html.Attributes
import Layout exposing (Layout, footer, phoneMenu, topMenu)
import Router.Routes exposing (Page(..), toPath)
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
view { device, model, showNavMenu } =
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
        , bio = Just "As CEO, Kenton Jarvie is responsible for guiding the overall strategic direction and success of the company."
        , linkedin = Just "https://www.linkedin.com/in/kentonjarvie/"
        , url = "static/headshots/kenton-sm.jpg"
        }
    , Member
        { name = "Marianne Cabalida"
        , position = "Executive Assistant"
        , bio = Just "Marianne Cabalida, as the Executive Assistant, ensures that the executive operations run smoothly and efficiently."
        , linkedin = Just "https://www.linkedin.com/in/mariannejcab/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Anson Kung"
        , position = "COO"
        , bio = Just "Anson Kung, as COO, focuses on the daily operation of the company and works closely with the CEO on strategic initiatives."
        , linkedin = Just "https://www.linkedin.com/in/ansonkung/"
        , url = "static/headshots/anson-sm.jpg"
        }
    , Member
        { name = "Neil  Prigge"
        , position = "Head of Partnerships"
        , bio = Just "Neil Prigge leads the company's partnership efforts. He manages the ongoing partnerships with Flint's clients, ensuring alignment and success in collaboration."
        , linkedin = Just "https://www.linkedin.com/in/neil-prigge/"
        , url = "static/headshots/neil-sm.jpg"
        }
    , Member
        { name = "Teresa Fisher"
        , position = "Partnership Executive"
        , bio = Just "Teresa Fisher is responsible for executing partnership strategies and maintaining valuable relationships with hospitals across the US."
        , linkedin = Just "https://www.linkedin.com/in/teresa-fisher"
        , url = "static/headshots/teresa-sm.jpg"
        }
    , Member
        { name = "Barry Borrilez"
        , position = "Nurse Staffing Director"
        , bio = Just "Barry Borrilez leads business development initiatives, focusing on creating and sustaining growth opportunities for the company."
        , linkedin = Just "https://www.linkedin.com/in/barry-borrilez/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Katherine Hooks"
        , position = "Nursing Educator"
        , bio = Just "Katherine Hooks, a Nursing Educator, is responsible for educating and training nursing staff to ensure the highest standards of care."
        , linkedin = Just "https://www.linkedin.com/in/katherine-hooks-7716579b/"
        , url = "static/headshots/katherine-sm.jpg"
        }
    , Member
        { name = "Olivia Renaud"
        , position = "Head of Product"
        , bio = Just "Olivia Renaud, as Head of Product, is responsible for product management and overseeing product direction, steering the alignment with customer value and organizational goals."
        , linkedin = Just "https://www.linkedin.com/in/oliviarenaud/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Isabelle Soares"
        , position = "Product Designer"
        , bio = Just "Isabelle Soares, as a Product Designer, plays a crucial role in designing user-friendly and aesthetically pleasing products."
        , linkedin = Just "https://www.linkedin.com/in/isabelle-soares-649805106/"
        , url = "static/headshots/isabelle-sm.jpg"
        }
    , Member
        { name = "Montse del Toro"
        , position = "Senior Nurse Success Manager"
        , bio = Just "Montse del Toro manages the success of nursing staff, ensuring proper training, support, and guidance in their roles."
        , linkedin = Just "https://www.linkedin.com/in/montserrat-del-toro/"
        , url = "static/headshots/montse-sm.jpg"
        }
    , Member
        { name = "Samuel Adedayo"
        , position = "Nurse Success Advisor"
        , bio = Just "Samuel Adedayo advises and supports nursing staff, playing a critical role in their ongoing success and development."
        , linkedin = Just "https://www.linkedin.com/in/samuel-adedayo-62b479145/"
        , url = "static/headshots/samuel-sm.jpg"
        }
    , Member
        { name = "Shelby LeBel"
        , position = "Nurse Success Advisor"
        , bio = Just "Shelby LeBel supports nurses in their career development, providing insights and advice to help them succeed."
        , linkedin = Nothing
        , url = "static/headshots/shelby-sm.jpg"
        }
    , Member
        { name = "Chelsea Mansour"
        , position = "Nurse Success Advisor"
        , bio = Just "Chelsea Mansour helps nurses achieve success in their roles, offering expert guidance and support."
        , linkedin = Nothing
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Jasleen Bhandal"
        , position = "Operations Associate"
        , bio = Just "Jasleen Bhandal assists with the daily operations of the company, ensuring efficiency and effectiveness in all processes."
        , linkedin = Just "https://www.linkedin.com/in/jasleen-bhandal/"
        , url = "static/headshots/jasleen-sm.jpg"
        }
    , Member
        { name = "Simon Green"
        , position = "VP Product"
        , bio = Just "Simon Green, as VP of Product, oversees the product strategy and development, driving innovation and user satisfaction."
        , linkedin = Just "https://www.linkedin.com/in/sg63"
        , url = "static/headshots/simon-sm.jpg"
        }
    , Member
        { name = "Jimmy Yao"
        , position = "Software Engineer"
        , bio = Just "Jimmy Yao, a Software Engineer, works on developing and maintaining high-quality software products."
        , linkedin = Just "https://www.linkedin.com/in/jimmy-yao-277a71232/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Azizul Karim"
        , position = "Software Engineer"
        , bio = Just "Azizul Karim, as a Software Engineer, contributes to the technical development, ensuring robust and scalable solutions."
        , linkedin = Nothing
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Gaël Deest"
        , position = "Senior Software Engineer"
        , bio = Just "Gaël Deest, a Senior Software Engineer, plays a key role in the development and maintenance of software products, ensuring excellence and innovation."
        , linkedin = Just "https://www.linkedin.com/in/ga%C3%ABl-deest-107a3650/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Handré Stolp"
        , position = "Senior Software Engineer"
        , bio = Just "Handré Stolp, as a Senior Software Engineer, plays a crucial role in developing and maintaining software applications, ensuring technical excellence and innovation."
        , linkedin = Just "https://www.linkedin.com/in/hanstolpo/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Łukasz Gołębiewski"
        , position = "Senior Software Engineer"
        , bio = Just "Łukasz Gołębiewski, a Senior Software Engineer, focuses on creating robust and scalable software solutions, aligning with the company's technology goals."
        , linkedin = Just "https://www.linkedin.com/in/lukasz--g/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Christof Schramm"
        , position = "Senior Software Engineer"
        , bio = Just "Christof Schramm, as a Senior Software Engineer, is instrumental in designing, coding, and testing software products, ensuring quality and alignment with the company’s objectives."
        , linkedin = Just "https://www.linkedin.com/in/christof-schramm-534731b9/"
        , url = "static/headshots/coming.svg"
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
