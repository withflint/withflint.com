module AboutUs.View exposing (view)

import AboutUs.Types exposing (Model, Msg(..))
import Device
import Element
    exposing
        ( Element
        , alignRight
        , alignTop
        , centerX
        , column
        , el
        , fill
        , fillPortion
        , height
        , html
        , htmlAttribute
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , rgb255
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
import Element.Font as Font exposing (letterSpacing)
import Html
import Html.Attributes as HtmlAttr
import Layout exposing (Layout, phoneMenu)
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, css, hf, lineHeight, maxW, minW, palette, pb, pt, wf)


view : Device.Device -> Model -> Layout Msg
view device model =
    let
        render view_ =
            -- Render with phoneMenu
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
                , height fill
                ]
                [ header device model
                , body
                ]
            ]
    , tablet =
        render <|
            [ column
                [ wf
                , height fill
                ]
                [ header device model
                , body
                ]
            ]
    , desktop =
        render <|
            [ column
                [ wf
                , height fill
                ]
                [ header device model
                , body
                ]
            ]
    }


body : Element Msg
body =
    column
        [ Background.color colors.cremeDark
        , wf
        , hf
        , Font.family [ Font.typeface "Inter" ]
        ]
        [ row
            [ wf ]
            [ row [ width <| fillPortion 2 ] [ Element.none ]
            , column [ width <| fillPortion 8 ]
                [ whoWeAre
                , team
                ]
            , row [ width <| fillPortion 2 ] [ Element.none ]
            ]
        ]



--- WHO WE ARE --


whoWeAre : Element msg
whoWeAre =
    let
        titleStyle =
            [ Font.center
            , Font.size 28
            , Font.semiBold
            , Font.color palette.primary
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


aboutTeam : Element msg
aboutTeam =
    paragraph [ hf, lineHeight 1.6, minW 300 ]
        [ text "As agents of change, we are deeply aligned with the healthcare industry's motivation.  We enable people to give their best by removing pain points and obstacles. Our team has experienced the hardships of the medical industry and immigration but also knows the silver lining of great work, caring for others, and finding community."
        ]


aboutFlint : Element msg
aboutFlint =
    column [ wf, spacingXY 0 12 ]
        [ paragraph [ lineHeight 1.6, minW 300 ]
            [ text "Flint was created to make the world a better place by connecting and improving lives. From those struggling to get adequate healthcare, to the nurses working two jobs in Nigeria. We get it." ]
        , paragraph [ lineHeight 1.6, minW 300 ]
            [ text "We consider ourselves agents of change for the future of nursing. Flint couples technology with insights and expertise — a winning combination." ]
        , paragraph [ lineHeight 1.6, minW 300 ]
            [ text "Flint is venture capital backed by the same people who funded the likes of AirBnB, Doordash & Instacart. Trust us to build long lasting solutions and partnerships within the healthcare industry." ]
        ]



--- TEAM ---


team : Element Msg
team =
    let
        titleStyle =
            [ Font.center
            , Font.size 28
            , Font.semiBold
            , Font.color palette.primary
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
                [ text "Team" ]
            ]
        , wrappedRow [ wf, hf, pt 80, spaceEvenly ]
            [ personCard kenton
            , personCard anson
            , personCard teresa
            ]
        , wrappedRow [ wf, hf, pt 80, spaceEvenly ]
            [ personCard neil
            , personCard kristi
            , personCard vanessa
            ]
        , wrappedRow [ wf, hf, pt 80, spaceEvenly ]
            [ personCard katherine
            , personCard simon
            , personCard montse
            , personCard fred
            ]
        ]


type alias Profile =
    { name : String
    , position : String
    , info : String -- bio/role
    , url : String -- ideal pic 408x397
    }


kenton : Profile
kenton =
    { name = "Kenton Jarvie"
    , position = "CEO"
    , info = loremIpsum
    , url = "static/images/kenton_sm.jpg"
    }


anson : Profile
anson =
    { name = "Anson Kung"
    , position = "COO"
    , info = loremIpsum
    , url = "static/images/anson_sm.jpg"
    }


teresa : Profile
teresa =
    { name = "Teresa Fisher"
    , position = "Partnerships"
    , info = loremIpsum
    , url = "static/images/teresa_sm.jpg"
    }


neil : Profile
neil =
    { name = "Neil Prigge"
    , position = "Partnerships"
    , info = loremIpsum
    , url = "static/images/neil_sm.jpg"
    }


kristi : Profile
kristi =
    { name = "Kristi Crawford"
    , position = "Immigration and Legal"
    , info = loremIpsum
    , url = "static/images/kristi_sm.jpg"
    }


vanessa : Profile
vanessa =
    { name = "Vanessa Teed"
    , position = "Product Manager"
    , info = loremIpsum
    , url = "static/images/vanessa_sm.jpg"
    }


katherine : Profile
katherine =
    { name = "Katherine Hooks"
    , position = "Nurse Educator"
    , info = loremIpsum
    , url = "static/images/katherine_sm.jpg"
    }


simon : Profile
simon =
    { name = "Simon Green"
    , position = "Head of Product"
    , info = loremIpsum
    , url = "static/images/simon_sm.jpg"
    }


montse : Profile
montse =
    { name = "Montserrat del Toro"
    , position = "Nurse Success Advisor"
    , info = loremIpsum
    , url = "static/images/montse_sm.jpg"
    }


fred : Profile
fred =
    { name = "Fred Varas"
    , position = "Partnerships Manager"
    , info = loremIpsum
    , url = "static/images/fred_sm.jpg"
    }


loremIpsum : String
loremIpsum =
    "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore"


personCard : Profile -> Element Msg
personCard profile =
    row
        [ wf
        , hf
        , paddingEach { bottom = 20, top = 0, right = 40, left = 0 }
        ]
        [ column
            [ wf
            , hf
            , minW 298
            , maxW 408
            , Background.color palette.white
            , Border.rounded 8
            ]
            [ row [ wf ]
                [ Element.image [ css "width" "100%", maxW 408, minW 298 ]
                    { src = profile.url
                    , description = ""
                    }
                ]
            , column [ paddingXY 24 24, spacingXY 0 12 ]
                [ -- NAME
                  paragraph [ Font.size 24, Font.color palette.primary, Font.semiBold ] [ text profile.name ]

                -- POSITION
                , paragraph [ Font.size 18, Font.color (rgb255 0 0 0), Font.semiBold ] [ text profile.position ]

                -- , paragraph [ Font.size 16, lineHeight 1.4, letterSpacing 1.8 ] [ text profile.info ]
                ]
            ]
        ]


header : Device.Device -> Model -> Element Msg
header device model =
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
                                , Font.color palette.white
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
                [ row ([ wf, centerX, Font.size rs.titleFontSize ] ++ Styles.title)
                    [ paragraph [ Font.center, Font.size rs.titleFontSize ] [ text title ] ]
                ]

            -- GAP
            , case device of
                Device.Phone _ ->
                    Element.none

                _ ->
                    row [ wf, height <| fillPortion 2 ] []
            ]
        ]
