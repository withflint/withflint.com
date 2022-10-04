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
import Html.Attributes as HtmlAttr
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
                [ row [ wf, hf, css "position" "relative" ] [ header device model ]
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
                [ row [ wf, hf ] [ header device model ]
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
                [ row [ wf, hf ] [ header device model ]
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
        { name = "Kristi Crawford"
        , position = "Head of Legal Affairs"
        , bio = Nothing
        , linkedin = Just "https://www.linkedin.com/in/kristi-l-crawford-6a1a5017"
        , url = "static/headshot/kristi-sm.jpg"
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
        , position = "Head of Product"
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
        , position = "Partnerships Manager"
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


header : Device.Device -> Model -> Element Msg
header device model =
    let
        gap x =
            row [ width <| fillPortion x ] []

        bg =
            [ css "background" "#FFDCC9"
            , css "background" "linear-gradient(281.5deg, #FFDCC9 -0.43%, #C8BCC7 8.22%, #8284AF 27.81%, #6E74A9 52.4%, #6359A1 82.46%)"
            ]

        blobSrc =
            "/static/images/header-blob-blue.svg"

        title =
            "Freedom. Equality. Quality."

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

        link ( label, page ) =
            Element.link
                []
                { url = toPath page
                , label =
                    el [ Font.center ] (text label)
                }

        responsiveFontSize =
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

        menu_ =
            row [ wf, height <| fillPortion 4 ]
                [ row [ wf ]
                    [ gap 7
                    , row
                        [ width <| fillPortion 4
                        , spacing 32
                        , Font.color colors.white
                        , Font.letterSpacing 2
                        , Font.size 14
                        ]
                        [ row [ alignRight, spacingXY 36 0 ]
                            (List.map (el (wf :: Styles.menu) << link) topMenu)
                        ]
                    , gap 2
                    ]
                ]

        title_ =
            row [ wf, height <| fillPortion 8 ]
                [ row ([ wf, centerX, Font.size responsiveFontSize.titleFontSize ] ++ Styles.title)
                    [ paragraph [ Font.center, Font.size responsiveFontSize.titleFontSize ] [ text title ] ]
                ]
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
                    menu_
            , title_
            , case device of
                Device.Phone _ ->
                    Element.none

                _ ->
                    row [ wf, height <| fillPortion 2 ] []
            ]
        ]


copy : { right : List String, left : List String }
copy =
    { right = [ "Flint is on a mission to fix American Healthcare by solving their biggest problem: not enough nurses. We do this by removing barriers for international nurses to immigrate and build a career in the United States, and we are on track to help a 1,000+ nurses immigrate by the end of next year (2023)." ]
    , left = [ "We’re a Y Combinator tech startup, with over $10M raised, and backed by Tier 1 investors such as Haystack, Audacious, and a list of powerful angels from company greats like Airbnb, Twitch and Flexport. Our team is fully remote across multiple countries, and comprised of industry veterans from technology, healthcare, and immigration." ]
    }
