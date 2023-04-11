module About.View exposing (view)

import About.Types exposing (Model, Msg(..))
import Device
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


view : Device.Device -> Model -> Layout Msg
view device model =
    let
        render view_ =
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
                [ row [ wf, hf, css "position" "relative" ] [ header device ]
                , column [ wf, hf, paddingXY 48 0 ] [ body ]
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
                [ row [ wf, hf ] [ header device ]
                , column [ wf, hf, paddingXY 48 0 ] [ body ]
                , column [ wf, pt 120 ] footer.phone
                ]
            ]
    , desktop =
        render <|
            [ column
                [ wf
                , Font.family [ Font.typeface "Inter" ]
                , css "position" "relative"
                , Background.color colors.cremeLight
                ]
                [ row [ wf, hf ] [ header device ]
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
                [ text "Working on the future of nursing" ]
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
        , bio = Nothing
        , linkedin = Just "https://www.linkedin.com/in/kentonjarvie"
        , url = "static/headshot/kenton-sm.jpg"
        }
    , Member
        { name = "Anson Kung"
        , position = "COO"
        , bio = Nothing
        , linkedin = Just "https://www.linkedin.com/in/ansonkung"
        , url = "static/headshot/anson-sm.jpg"
        }
    , Member
        { name = "Teresa Fisher"
        , position = "Partnership Executive"
        , bio = Nothing
        , linkedin = Just "https://www.linkedin.com/in/teresa-fisher"
        , url = "static/headshot/teresa-sm.jpg"
        }
    , Member
        { name = "Neil Prigge"
        , position = "Head of Partnerships"
        , bio = Nothing
        , linkedin = Just "https://www.linkedin.com/in/neil-prigge"
        , url = "static/headshot/neil-sm.jpg"
        }
    , Member
        { name = "Vanessa Teed"
        , position = "Product Manager"
        , bio = Nothing
        , linkedin = Just "https://www.linkedin.com/in/vanessa-teed-38b160248"
        , url = "static/headshot/vanessa-sm.jpg"
        }
    , Member
        { name = "Katherine Hooks"
        , position = "Nurse Educator"
        , bio = Nothing
        , linkedin = Just "https://www.linkedin.com/in/katherine-hooks-7716579b"
        , url = "static/headshot/katherine-sm.jpg"
        }
    , Member
        { name = "Simon Green"
        , position = "VP Product"
        , bio = Nothing
        , linkedin = Just "https://www.linkedin.com/in/sg63"
        , url = "static/headshot/simon-sm.jpg"
        }
    , Member
        { name = "Montserrat del Toro"
        , position = "Nurse Success Manager"
        , bio = Nothing
        , linkedin = Just "https://www.linkedin.com/in/montserrat-del-toro"
        , url = "static/headshot/montse-sm.jpg"
        }
    , Member
        { name = "Fred Varas"
        , position = "Director Product Partnerships"
        , bio = Nothing
        , linkedin = Just "https://www.linkedin.com/in/fred-varas-85862721a"
        , url = "static/headshot/fred-sm.jpg"
        }
    , Member
        { name = "Wonchan Kim"
        , position = "Special Projects"
        , bio = Nothing
        , linkedin = Just "https://www.linkedin.com/in/wonchankim"
        , url = "static/headshot/wonchan-sm.jpg"
        }
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

                bio =
                    case profile.bio of
                        Just t ->
                            paragraph [ Font.size 16, lineHeight 1.4, Font.letterSpacing 1.8 ] [ text t ]

                        Nothing ->
                            none

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
                    , column [ paddingXY 24 24, spacingXY 0 12, wf ]
                        [ row [ wf, spaceEvenly ] [ name, linkedin ]
                        , position
                        ]
                    , bio
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


header : Device.Device -> Element Msg
header device =
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
        }


copy : { right : List String, left : List String }
copy =
    { right = [ "Flint is on a mission to fix American Healthcare by solving their biggest problem: not enough nurses. We do this by removing barriers for international nurses to immigrate and build a career in the United States, and we are on track to help a 1,000+ nurses immigrate by the end of next year (2023)." ]
    , left = [ "We’re a Y Combinator tech startup, with over $10M raised, and backed by Tier 1 investors such as Haystack, Audacious, and a list of powerful angels from company greats like Airbnb, Twitch and Flexport. Our team is fully remote across multiple countries, and comprised of industry veterans from technology, healthcare, and immigration." ]
    }
