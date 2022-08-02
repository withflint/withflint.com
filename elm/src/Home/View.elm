module Home.View exposing (view)

import Device exposing (Device(..))
import Element
    exposing
        ( Attribute
        , Element
        , alignBottom
        , alignLeft
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
        , image
        , maximum
        , mouseOver
        , newTabLink
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font exposing (underline)
import Element.Input as Input
import Home.Types exposing (Model)
import Html
import Html.Attributes as HtmlAttr
import Layout exposing (Layout, footer, header)
import Styles exposing (buttons, colors, heading, hf, palette, pb, pl, pr, pt)


view : Model -> Device -> Layout msg
view _ device =
    { phone =
        [ column
            [ width <| maximum 1500 fill
            , height fill
            , centerX

            -- , paddingXY 20 40
            ]
            --header.phone
            --++
            (phoneView device
                ++ footer.phone
            )
        ]
    , tablet =
        [ column
            [ -- centerX
              width <| maximum 1500 fill
            , height fill
            , wf

            -- , paddingXY 100 40
            ]
            --header.tablet
            --++
            (desktopView device
                ++ homeFooter device
             -- ++ footer.tablet
            )
        ]
    , desktop =
        [ column
            [ centerX

            --   width <| maximum 1500 fill
            , wf
            , height fill

            -- , paddingXY 100 40
            ]
            -- header.desktop
            -- ++
            (desktopView device
                ++ homeFooter device
             -- ++ footer.desktop
            )
        ]
    }


phoneView : Device -> List (Element msg)
phoneView device =
    let
        viewport =
            case device of
                Phone vp ->
                    vp

                Tablet vp ->
                    vp

                Desktop vp ->
                    vp

                NotSet ->
                    { width = 0, height = 0 }

        heroImg =
            html <|
                Html.div
                    []
                    [ Html.div
                        [ HtmlAttr.style "position" "relative"
                        , HtmlAttr.style "width" "100vw"
                        , HtmlAttr.style "overflow" "hidden"
                        ]
                        [ Html.img
                            [ HtmlAttr.src "/static/images/home-hero-blob.svg"
                            , HtmlAttr.style "width" (String.fromInt viewport.width)
                            , HtmlAttr.style "position" "absolute"

                            -- , HtmlAttr.style "bottom" "-120px"
                            -- , HtmlAttr.style "right" "-40px"
                            , HtmlAttr.style "bottom" (String.fromInt viewport.width)
                            , HtmlAttr.style "right" "-40px"
                            , HtmlAttr.style "z-index" "1"
                            ]
                            []
                        , Html.div
                            [ HtmlAttr.style "display" "flex"
                            , HtmlAttr.style "justify-content" "center"
                            ]
                            [ Html.img
                                [ HtmlAttr.src "/static/images/home-portrait-nurse.png"
                                , HtmlAttr.style "width" "inherit"
                                , HtmlAttr.style "z-index" "2"
                                ]
                                []
                            ]
                        ]
                    ]

        logo : Element msg
        logo =
            Element.image [ width (px 110), height (px 54) ] { src = "/static/images/logo.svg?new", description = "Flint" }
    in
    [ column [ wf, hf, Background.color colors.cremeDark, pt 36 ]
        [ -- HERO
          column [ wf ]
            [ -- HERO TEXT
              column [ wf, spacingXY 0 14, pl 24 ]
                [ paragraph
                    [ Font.color palette.primary
                    , Font.semiBold
                    , Font.size 42
                    ]
                    [ text "It's all about people, with" ]
                , logo
                ]

            -- HERO IMG
            , heroImg
            ]

        -- CARD
        , column
            [ wf ]
            [ card device
                { title = "Need a long-term nurse?"
                , desc = "Recreate the way you hire nurses"
                , btnLabel = "Flint for hospitals"
                , bg = industryBg
                }
            , card device
                { title = "Want to be a nurse in America?"
                , desc = "Find support and community from start to finish"
                , btnLabel = "Flint for nurses"
                , bg = nursesBg
                }
            ]
        ]
    ]


card : Device -> { title : String, desc : String, btnLabel : String, bg : List (Attribute msg) } -> Element msg
card device { title, desc, btnLabel, bg } =
    let
        responsiveSize =
            case device of
                Phone _ ->
                    { cardHeight = 200, btnTopPadding = 24 }

                Desktop _ ->
                    { cardHeight = 300, btnTopPadding = 44 }

                Tablet _ ->
                    { cardHeight = 300, btnTopPadding = 44 }

                NotSet ->
                    { cardHeight = 300, btnTopPadding = 44 }

        btn =
            [ Border.roundEach { topLeft = 16, topRight = 0, bottomRight = 16, bottomLeft = 0 }

            -- , Border.color palette.white
            -- , Border.width 1
            , padding 10

            -- , Font.color palette.white
            , Font.color palette.primary
            , Font.semiBold
            , Font.size 16
            , Background.color colors.cremeDark

            -- , Background.color bgColor
            -- , width (px 128)
            -- , height (px 36)
            , paddingEach { top = 10, right = 19, bottom = 10, left = 22 }
            , Font.regular
            , mouseOver
                [ Font.color colors.cremeLight
                , Background.color colors.carminePink
                , Border.color colors.carminePink
                ]
            ]
    in
    column
        ([ wf, paddingXY 0 32 ]
            ++ bg
        )
        [ column
            [ height (px responsiveSize.cardHeight)
            , Font.color palette.white
            , centerX
            ]
            [ column [ wf, centerX, centerY ]
                [ column [ pl 24 ]
                    [ paragraph
                        [ Font.size 26
                        , Font.semiBold
                        ]
                        [ text <| title ]
                    , paragraph
                        [ Font.family [ Font.typeface "Inter" ]
                        , Font.size 16
                        , pt 12
                        ]
                        [ text <| desc ]
                    , el [ pt responsiveSize.btnTopPadding, wf ]
                        (Input.button
                            (centerY :: centerX :: wf :: Font.size 15 :: btn)
                            { onPress = Nothing
                            , label = paragraph [ Font.center ] [ text <| btnLabel ]
                            }
                        )
                    ]
                ]
            ]
        ]


industryBg : List (Attribute msg)
industryBg =
    [ htmlAttribute <| HtmlAttr.style "background" "rgb(68,55,109)"
    , htmlAttribute <| HtmlAttr.style "background" "linear-gradient(281.17deg, #A7C8F9 -8.91%, #8494C7 12.48%, #6E74A9 42.43%, #626297 82.36%)"
    ]


nursesBg : List (Attribute msg)
nursesBg =
    [ htmlAttribute <| HtmlAttr.style "background" "rgb(229,72,72)"
    , htmlAttribute <| HtmlAttr.style "background" "linear-gradient(282.96deg, #E54848 -0.52%, #BA4352 8.17%, #7E3D60 37.38%, #5D3968 66.24%)"

    -- , htmlAttribute <| HtmlAttr.style "transform" "rotate(-180deg)"
    ]



-- type alias Element msg =
--     List (Element.Attribute msg) -> List (Element.Element msg)
-- card : Element msg -> { title : String, desc : String, btnLabel : String } -> Element msg
-- card el info =
--     el <|
--         []
--             []
-- phoneView : List (Element msg)
-- phoneView =
--     [ column
--         [ wf
--         , paddingXY 0 0
--         , spacingXY 0 20
--         ]
--         [ column [ wf, height fill, paddingXY 0 0, spacing 20 ]
--             [ column [ wf, spacing 20 ]
--                 [ image [ wf ] { src = "/static/images/top2.png", description = "Registered Nurses" }
--                 , row [ centerX, paddingXY 10 0 ] [ paragraph (heading ++ [ Font.size 30, Font.center, width (fill |> maximum 300) ]) [ text copy.topHeading ] ]
--                 , meetWithExpertCta
--                 --
--                 , h2 copy.flintCanHelp
--                 , orderedList copy.flintCanHelpList
--                 --
--                 , h2 copy.staffing
--                 , p copy.staffingParagraph
--                 , meetWithExpertCta
--                 --
--                 , h2 copy.concierge
--                 , p copy.conciergeParagraph1
--                 , p copy.conciergeParagraph2
--                 , orderedList copy.conciergeList
--                 , meetWithExpertCta
--                 ]
--             ]
--         ]
--     ]


desktopView : Device -> List (Element msg)
desktopView device =
    let
        heroImg =
            row
                [ centerX
                , htmlAttribute <| HtmlAttr.style "position" "relative"
                , htmlAttribute <| HtmlAttr.style "overflow" "hidden"
                ]
                [ column
                    [ wf
                    , htmlAttribute <| HtmlAttr.style "z-index" "2"
                    ]
                    [ paragraph heroTitleAttr [ text "It's all about people," ]
                    , row []
                        [ paragraph heroTitleAttr [ text "with" ]
                        , Element.image [ width (px 114), height (px 48) ] { src = "/static/images/logo.svg?new", description = "Flint" }
                        ]
                    ]
                , Element.image
                    [ -- width (px 114), height (px 48)
                      htmlAttribute <| HtmlAttr.style "z-index" "3"
                    ]
                    { src =
                        "/static/images/home-portrait-nurse.png"
                    , description = "Flint"
                    }
                , html <|
                    Html.img
                        [ HtmlAttr.src "/static/images/home-hero-blob.svg"
                        , HtmlAttr.style "width" "90%"
                        , HtmlAttr.style "position" "absolute"
                        , HtmlAttr.style "bottom" "-140px"
                        , HtmlAttr.style "right" "-16px"
                        , HtmlAttr.style "z-index" "1"
                        ]
                        []
                ]

        heroTitleAttr =
            [ Font.color palette.primary
            , Font.semiBold
            , Font.size 42
            ]

        -- html <|
        --     Html.div
        --         [ HtmlAttr.style "width" "100vw"
        --         ]
        --         [ Html.div
        --             [ HtmlAttr.style "position" "relative"
        --             , HtmlAttr.style "width" "100vw"
        --             , HtmlAttr.style "overflow" "hidden"
        --             ]
        --             [ Html.img
        --                 [ HtmlAttr.src "/static/images/home-hero-blob.svg"
        --                 , HtmlAttr.style "width" "70%"
        --                 , HtmlAttr.style "position" "absolute"
        --                 , HtmlAttr.style "bottom" "-120px"
        --                 , HtmlAttr.style "right" "100px"
        --                 , HtmlAttr.style "z-index" "1"
        --                 ]
        --                 []
        --             , Html.div
        --                 [ HtmlAttr.style "display" "flex"
        --                 , HtmlAttr.style "justify-content" "center"
        --                 ]
        --                 [ Html.div []
        --                     -- [ Html.span [] [ Html.text "It's all about people," ]
        --                     -- , Html.span [] [ Html.text "with" ]
        --                     -- , Html.img
        --                     --     [ HtmlAttr.src "/static/images/logo.svg?new"
        --                     --     , HtmlAttr.style "width" "100%"
        --                     --     ]
        --                     --     []
        --                     -- ]
        --                     []
        --                 , Html.img
        --                     [ HtmlAttr.src "/static/images/home-portrait-nurse.png"
        --                     , HtmlAttr.style "width" "inherit"
        --                     , HtmlAttr.style "z-index" "2"
        --                     ]§
        --                     []
        --                 ]
        --             ]
        --         ]
    in
    [ column [ pt 48, wf, hf, Background.color colors.cremeDark ]
        [ row [ spacing 48, centerX, Font.color palette.primary, Font.semiBold ]
            [ el [] (text "Partnerships")
            , el [] (text "Nurse Careers")
            , el [] (text "Blog")
            ]
        , row [ pt 72, wf ]
            [ heroImg
            ]
        , row [ wf, Background.color colors.blue1 ]
            [ card device
                { title = "Need a long-term nurse?"
                , desc = "Recreate the way you hire nurses"
                , btnLabel = "Flint for hospitals"
                , bg = industryBg
                }
            , card device
                { title = "Want to be a nurse in America?"
                , desc = "Find support and community from start to finish"
                , btnLabel = "Flint for nurses"
                , bg = nursesBg
                }
            ]
        ]
    ]


homeFooter : Device -> List (Element msg)
homeFooter device =
    [ row [ wf, alignBottom, Background.color colors.cremeDark ]
        [ row [ width <| fillPortion 2 ] []
        , column [ width <| fillPortion 1, wf ]
            [ row []
                [ row []
                    [ Element.image [ width (px 80), height (px 24) ] { src = "/static/images/logo.svg?new", description = "Flint" }
                    ]
                , row
                    [ width (px 370) ]
                    [ paragraph [ wf, Font.center ] [ text "About Us" ]
                    , paragraph [ wf, Font.center ] [ text "Join the Team" ]
                    , paragraph [ wf, Font.center ] [ text "Contact Us" ]
                    ]
                ]
            , row [ width fill, Font.size 10, paddingXY 24 24 ]
                [ row [ spacing 20 ] [ text "© 2022 Flint, all rights reserved", newTabLink [ underline ] { url = "/privacy", label = text "Privacy Policy" } ] ]
            ]
        , row [ width <| fillPortion 2 ] []
        ]
    ]



-- desktopView : List (Element msg)
-- desktopView =
--     [ column
--         [ wf
--         -- , paddingXY 0 50
--         -- , spacingXY 0 20
--         ]
--         [ column [ wf, height fill, paddingXY 0 0, spacing 20 ]
--             [ column [ wf, spacing 100, height fill ]
--                 [ row [ wf, height fill ]
--                     [ column [ wf, spacing 20, alignTop ]
--                         [ column [ wf, height fill, centerY, paddingXY 0 150, spacing 30, paddingEach { top = 150, bottom = 150, left = 0, right = 10 } ]
--                             [ paragraph [ Font.size 30, width (fill |> maximum 500), height fill, centerY ] [ text "Securing nurses for your future" ]
--                             , paragraph Styles.paragraph [ text "The right nurse. Health care expertise on demand. Find everything you need to optimize your healthcare staffing needs — with Flint." ]
--                             , meetWithExpertTop
--                             ]
--                         ]
--                     , image [ wf ] { src = "/static/images/top2.png", description = "Registered Nurses" }
--                     ]
--                 --
--                 , pair
--                     [ h2w copy.flintCanHelp
--                     , orderedList copy.flintCanHelpList
--                     , expand
--                     , meetWithExpertTop
--                     ]
--                     --
--                     [ h2w copy.staffing
--                     , pw copy.staffingParagraph
--                     , expand
--                     , meetWithExpertTop
--                     ]
--                 --
--                 , column [ wf ]
--                     [ h2w copy.concierge
--                     , pw copy.conciergeParagraph1
--                     , pw copy.conciergeParagraph2
--                     , orderedList copy.conciergeList
--                     , meetWithExpertTop
--                     ]
--                 ]
--             ]
--         ]
--     ]


meetWithExpertCta : Element msg
meetWithExpertCta =
    cta ( "https://calendly.com/montse-withflint/flintconsultation", "Meet With Us" )


meetWithExpertTop : Element msg
meetWithExpertTop =
    leftCta ( "https://calendly.com/montse-withflint/flintconsultation", "Meet With Us" )


cta : ( String, String ) -> Element msg
cta ( url, t ) =
    column ([ wf, centerX ] ++ Styles.paragraph ++ [ alignBottom ])
        [ newTabLink ([ wf, centerX, width (maximum 200 fill), Font.center, mouseOver [ Background.color colors.blue1 ] ] ++ buttons.primary)
            { url = url, label = text t }
        ]


leftCta : ( String, String ) -> Element msg
leftCta ( url, t ) =
    column ([ wf, centerX ] ++ Styles.paragraph)
        [ newTabLink ([ wf, width (maximum 200 fill), Font.center, mouseOver [ Background.color colors.blue1 ] ] ++ buttons.primary)
            { url = url, label = text t }
        ]


orderedList : List ( String, String ) -> Element msg
orderedList list =
    column [ wf, spacing 20, paddingXY 30 30 ] (list |> List.map (\( n, t ) -> row [ spacing 5 ] [ el [ alignTop ] (text n), paragraph [ alignTop ] [ text t ] ]))


copy : { topHeading : String, flintCanHelp : String, flintCanHelpList : List ( String, String ), staffing : String, staffingParagraph : String, concierge : String, conciergeParagraph1 : String, conciergeParagraph2 : String, conciergeList : List ( String, String ), standards : String, standardsParagraph : String, best : String, bestParagraph : String, fast : String, fastParagraph : String, guarantee : String, guaranteeParagraph : String, team : String, teamParagraph : String, secure : String, secureParagraph : String }
copy =
    { topHeading =
        "Securing Nurses for Your Future"
    , flintCanHelp = "Flint Can Help"
    , flintCanHelpList =
        [ ( "• ", "Decrease your staffing costs by over 50%" )
        , ( "• ", "Remove your reliance on travel nurses" )
        , ( "• ", "Recruit incredible nurses with 3-10 years of experience (employed directly by your organization)" )
        , ( "• ", "Build a long term recruitment funnel to never be short nurses again" )
        , ( "• ", "Improve your quality of care with passionate nurses" )
        ]
    , staffing = "Your Long Term Staffing Solution"
    , staffingParagraph = "At Flint, our goal is to help you secure your long term staffing needs. We help by sourcing the best registered nurses from around the globe, providing you with a steady and reliable stream of experienced nurses who are employed directly by your organization. "
    , concierge = "Fully Managed Concierge Experience"
    , conciergeParagraph1 = "We understand that you’re very busy and have a lot on your plate. That’s why you can expect us to fully manage the entire recruiting, licensing, immigration, and relocation process. "
    , conciergeParagraph2 = "All you have to do is select the candidates you want to hire and onboard them when they walk into your door. We’ll do all the heavy lifting throughout the process, including:"
    , conciergeList =
        [ ( "• ", "Candidate sourcing" )
        , ( "• ", "Candidate screening" )
        , ( "• ", "Credential verification" )
        , ( "• ", "NCLEX Preparation" )
        , ( "• ", "Language examination" )
        , ( "• ", "Licensing in the state of practice" )
        , ( "• ", "Immigration management" )
        , ( "• ", "Representation with the government" )
        , ( "• ", "Arrival and settlement assistance" )
        , ( "• ", "Travel assistance" )
        , ( "• ", "Support for family members" )
        , ( "• ", "Dedicated relocation & cultural advisor for the nurse (3 years)" )
        ]
    , standards = "Nurses that meet the highest standards"
    , standardsParagraph = "Every nurse we recruit is carefully screened and vetted by our Nurse Success to ensure they can communicate effectively in English, have the appropriate experience for your needs, and possess the right attitude to succeed in the United States. When it comes to the final selection, you’re in control. Our team will work alongside yours to facilitate the selection process, and present candidates tailored to your specific hiring needs."
    , best = "We Source the Best Nurses"
    , bestParagraph = "Flint has the best nurses. Just ask anyone who’s met our candidates. With an international sourcing team that sources from the entire world, we receive a lot of applications, thousands per year in fact. With such a large pool of nurses, we’re able to screen and select the best candidates for your needs."
    , fast = "Fastest Immigration Possible"
    , fastParagraph = "Most firms will only try one immigration path. At Flint, our in-house legal team, empowered by custom built computer technology, designs personalized plans for each nurse. We leverage many possible paths, including lotteries, to get your nurse working ASAP. "
    , guarantee = "Hospital Guarantee"
    , guaranteeParagraph = "If the nurse just isn’t working out within your organization and you choose to end the employment, we’ll provide you with a prorated refund. We can offer this guarantee because our nurses are such strong candidates. "
    , team = "An Experienced and Proven Team"
    , teamParagraph = "We’re a veteran team of health care professionals, immigration lawyers, and engineers who are passionate about solving the nursing shortage in America while removing barriers for nurses to immigrate and build a new life in our country. With experience immigrating and placing hundreds of nurses across 40+ states, our team stands ready to help you succeed. "
    , secure = "Secure your nurses"
    , secureParagraph = "To learn more about how easy it is to work with Flint. Set up a meeting with one of our partnership managers to get started."
    }


h2 : String -> Element msg
h2 t =
    paragraph
        [ centerX
        , paddingXY 10 10
        , width
            (fill
                |> maximum 300
            )
        , Font.size 30
        , Font.center
        ]
        [ text t ]


h2w : String -> Element msg
h2w t =
    paragraph
        [ paddingXY 10 10
        , Font.size 30
        ]
        [ text t ]


p : String -> Element msg
p t =
    paragraph
        [ centerX
        , paddingXY 10 10
        , width
            (fill
                |> maximum 550
            )
        , spacing 15
        ]
        [ text t ]


pw : String -> Element msg
pw t =
    paragraph
        [ centerX
        , paddingXY 10 10
        , spacing 15
        ]
        [ text t ]


wf : Element.Attribute msg
wf =
    width fill


cell : List (Element msg) -> Element msg
cell =
    column [ wf, alignTop, height fill ]


pair : List (Element msg) -> List (Element msg) -> Element msg
pair l1 l2 =
    row [ wf, alignTop, spacing 100 ] [ cell l1, cell l2 ]


expand : Element msg
expand =
    row [ height fill, wf ] [ text "" ]
