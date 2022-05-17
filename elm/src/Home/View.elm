module Home.View exposing (view)

import Element
    exposing
        ( Element
        , alignBottom
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , image
        , maximum
        , mouseOver
        , newTabLink
        , paddingEach
        , paddingXY
        , paragraph
        , row
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Home.Types exposing (Model)
import Layout exposing (Layout, footer, header)
import Styles exposing (buttons, colors, heading)


view : Model -> Layout msg
view _ =
    { phone =
        [ column
            [ centerX
            , width <| maximum 1500 fill
            , height fill
            , paddingXY 20 40
            ]
            (header.phone
                ++ phoneView
                ++ footer.phone
            )
        ]
    , tablet =
        [ column
            [ centerX
            , width <| maximum 1500 fill
            , height fill
            , paddingXY 100 40
            ]
            (header.tablet
                ++ desktopView
                ++ footer.tablet
            )
        ]
    , desktop =
        [ column
            [ centerX
            , width <| maximum 1500 fill
            , height fill
            , paddingXY 100 40
            ]
            (header.desktop
                ++ desktopView
                ++ footer.desktop
            )
        ]
    }


phoneView : List (Element msg)
phoneView =
    [ column
        [ wf
        , paddingXY 0 0
        , spacingXY 0 20
        ]
        [ column [ wf, height fill, paddingXY 0 0, spacing 20 ]
            [ column [ wf, spacing 20 ]
                [ image [ wf ] { src = "/static/images/top2.png", description = "Registered Nurses" }
                , row [ centerX, paddingXY 10 0 ] [ paragraph (heading ++ [ Font.size 30, Font.center, width (fill |> maximum 300) ]) [ text copy.topHeading ] ]
                , meetWithExpertCta

                --
                , h2 copy.flintCanHelp
                , orderedList copy.flintCanHelpList

                --
                , h2 copy.staffing
                , p copy.staffingParagraph
                , meetWithExpertCta

                --
                , h2 copy.concierge
                , p copy.conciergeParagraph1
                , p copy.conciergeParagraph2
                , orderedList copy.conciergeList
                , meetWithExpertCta
                ]
            ]
        ]
    ]


desktopView : List (Element msg)
desktopView =
    [ column
        [ wf
        , paddingXY 0 50
        , spacingXY 0 20
        ]
        [ column [ wf, height fill, paddingXY 0 0, spacing 20 ]
            [ column [ wf, spacing 100, height fill ]
                [ row [ wf, height fill ]
                    [ column [ wf, spacing 20, alignTop ]
                        [ column [ wf, height fill, centerY, paddingXY 0 150, spacing 30, paddingEach { top = 150, bottom = 150, left = 0, right = 10 } ]
                            [ paragraph [ Font.size 30, width (fill |> maximum 500), height fill, centerY ] [ text "Securing nurses for your future" ]
                            , paragraph Styles.paragraph [ text "The right nurse. Health care expertise on demand. Find everything you need to optimize your heathcare staffing needs — with Flint." ]
                            , meetWithExpertTop
                            ]
                        ]
                    , image [ wf ] { src = "/static/images/top2.png", description = "Registered Nurses" }
                    ]

                --
                , pair
                    [ h2w copy.flintCanHelp
                    , orderedList copy.flintCanHelpList
                    , expand
                    , meetWithExpertTop
                    ]
                    --
                    [ h2w copy.staffing
                    , pw copy.staffingParagraph
                    , expand
                    , meetWithExpertTop
                    ]

                --
                , column [ wf ]
                    [ h2w copy.concierge
                    , pw copy.conciergeParagraph1
                    , pw copy.conciergeParagraph2
                    , orderedList copy.conciergeList
                    , meetWithExpertTop
                    ]
                ]
            ]
        ]
    ]


meetWithExpertCta : Element msg
meetWithExpertCta =
    cta ( "https://calendly.com/anson-flint/intro-to-flint-healthcare-web", "Meet With Us" )


meetWithExpertTop : Element msg
meetWithExpertTop =
    leftCta ( "https://calendly.com/anson-flint/intro-to-flint-healthcare-web", "Meet With Us" )


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
    , standardsParagraph = "Every nurse we recruit is carefully screened and vetted by our Talent Team to ensure they can communicate effectively in English, have the appropriate experience for your needs, and possess the right attitude to succeed in the United States. When it comes to the final selection, you’re in control. Our team will work alongside yours to facilitate the selection process, and present candidates tailored to your specific hiring needs."
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
