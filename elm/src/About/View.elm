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
        , bio = Just "Overseeing investor relations, finance and people operations, he ensures Flint's direction aligns with its mission and values."
        , linkedin = Just "https://www.linkedin.com/in/kentonjarvie/"
        , url = "static/headshots/kenton-sm.jpg"
        }
    , Member
        { name = "Marianne Cabalida"
        , position = "Executive Assistant"
        , bio = Just "In her role Marianne oversees  ongoing accounting and people operations, making sure that it runs smoothly and efficiently."
        , linkedin = Just "https://www.linkedin.com/in/mariannejcab/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Teresa Fisher"
        , position = "Partnership Executive"
        , bio = Just "Teresa crafts new alliances with healthcare providers, driving Flint's business development and strategic outreach."
        , linkedin = Just "https://www.linkedin.com/in/teresa-fisher"
        , url = "static/headshots/teresa-sm.jpg"
        }
    , Member
        { name = "Barry Borrilez"
        , position = "Nurse Staffing Director"
        , bio = Just "Engaging with potential partners, the Nurse Staffing Director is at the forefront of solving staffing challenges in the healthcare landscape."
        , linkedin = Just "https://www.linkedin.com/in/barry-borrilez/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Olivia Renaud"
        , position = "Head of Product"
        , bio = Just "As Head of Product, emphasis is placed on aligning Flint's product direction with both customer value and organizational objectives."
        , linkedin = Just "https://www.linkedin.com/in/oliviarenaud/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Isabelle Soares"
        , position = "Product Designer"
        , bio = Just "Crafting user-centric designs, the Product Designer plays an integral role in shaping Flint's digital experiences."
        , linkedin = Just "https://www.linkedin.com/in/isabelle-soares-649805106/"
        , url = "static/headshots/isabelle-sm.jpg"
        }
    , Member
        { name = "Montse del Toro"
        , position = "Senior Nurse Success Manager"
        , bio = Just "Championing nurses throughout their journey with Flint and its partners, the Senior Nurse Success Manager offers unparalleled support and guidance."
        , linkedin = Just "https://www.linkedin.com/in/montserrat-del-toro/"
        , url = "static/headshots/montse-sm.jpg"
        }
    , Member
        { name = "Katherine Hooks"
        , position = "Nursing Educator"
        , bio = Just "With a commitment to excellence, the Nursing Educator equips nurses for success on the NCLEX examination and beyond."
        , linkedin = Just "https://www.linkedin.com/in/katherine-hooks-7716579b/"
        , url = "static/headshots/katherine-sm.jpg"
        }
    , Member
        { name = "Samuel Adedayo"
        , position = "Nurse Success Advisor"
        , bio = Just "Playing a pivotal role in the development and success of Flint's nurses, the Nurse Success Advisor offers valuable insights and mentorship."
        , linkedin = Just "https://www.linkedin.com/in/samuel-adedayo-62b479145/"
        , url = "static/headshots/samuel-sm.jpg"
        }
    , Member
        { name = "Shelby LeBel"
        , position = "Nurse Success Advisor"
        , bio = Just "Guiding nurses towards their full potential, the Nurse Success Advisor is a beacon of support and knowledge."
        , linkedin = Nothing
        , url = "static/headshots/shelby-sm.jpg"
        }
    , Member
        { name = "Chelsea Mansour"
        , position = "Nurse Success Advisor"
        , bio = Just "By providing tailored guidance and insights, the Nurse Success Advisor ensures Flint's nurses excel in their roles."
        , linkedin = Just "https://www.linkedin.com/in/chelsea-mansour-184600206/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Jasleen Bhandal"
        , position = "Operations Associate"
        , bio = Just "Overseeing nurse licensing operations, the Operations Associate plays a vital role in Flint's Nurse Success initiative."
        , linkedin = Just "https://www.linkedin.com/in/jasleen-bhandal/"
        , url = "static/headshots/jasleen-sm.jpg"
        }
    , Member
        { name = "Anson Kung"
        , position = "COO"
        , bio = Just "Leading the charge on nurse recruitment, he is dedicated to the expansion and diversification of Flint's nursing growth."
        , linkedin = Just "https://www.linkedin.com/in/ansonkung/"
        , url = "static/headshots/anson-sm.jpg"
        }
    , Member
        { name = "Neil  Prigge"
        , position = "Head of Partnerships"
        , bio = Just "Tasked with cultivating ongoing relationships, the Head of Partnerships ensures that Flint and its partners mutually thrive."
        , linkedin = Just "https://www.linkedin.com/in/neil-prigge/"
        , url = "static/headshots/neil-sm.jpg"
        }
    , Member
        { name = "Simon Green"
        , position = "VP Product"
        , bio = Just "As VP of Product, he oversees product strategy, development, and nurse success operations. He drives innovation, user satisfaction, and ensures the comprehensive support of nurses through product operations."
        , linkedin = Just "https://www.linkedin.com/in/sg63"
        , url = "static/headshots/simon-sm.jpg"
        }
    , Member
        { name = "Jimmy Yao"
        , position = "Software Engineer"
        , bio = Just "As a Software Engineer, works on developing and maintaining high-quality software products."
        , linkedin = Just "https://www.linkedin.com/in/jimmy-yao-277a71232/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Azizul Karim"
        , position = "Software Engineer"
        , bio = Just "Contributes to the technical development, ensuring robust and scalable solutions."
        , linkedin = Nothing
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Gaël Deest"
        , position = "Senior Software Engineer"
        , bio = Just "Plays a key role in the development and maintenance of software products, ensuring excellence and innovation."
        , linkedin = Just "https://www.linkedin.com/in/ga%C3%ABl-deest-107a3650/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Handré Stolp"
        , position = "Senior Software Engineer"
        , bio = Just "Plays a crucial role in developing and maintaining software applications, ensuring technical excellence and innovation."
        , linkedin = Just "https://www.linkedin.com/in/hanstolpo/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Łukasz Gołębiewski"
        , position = "Senior Software Engineer"
        , bio = Just "Focuses on creating robust and scalable software solutions, aligning with the company's technology goals."
        , linkedin = Just "https://www.linkedin.com/in/lukasz--g/"
        , url = "static/headshots/coming.svg"
        }
    , Member
        { name = "Christof Schramm"
        , position = "Senior Software Engineer"
        , bio = Just "Is instrumental in designing, coding, and testing software products, ensuring quality and alignment with the company’s objectives."
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
