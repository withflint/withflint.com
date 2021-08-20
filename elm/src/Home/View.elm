module Home.View exposing (view)

import Element
    exposing
        ( Element
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , image
        , link
        , maximum
        , mouseOver
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
import Router.Routes exposing (Page(..), toPath)
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
        [ width fill
        , paddingXY 0 0
        , spacingXY 0 20
        ]
        [ column [ width fill, height fill, paddingXY 0 0, spacing 20 ]
            [ column [ width fill, spacing 20 ]
                [ image [ width fill ] { src = "/static/images/top.svg", description = "Health care workers" }
                , row [ centerX, paddingXY 10 0 ] [ paragraph (heading ++ [ Font.size 30, Font.center, width (fill |> maximum 300) ]) [ text copy.topHeading ] ]
                , meetWithExpertCta
                , fullImage ( "/static/images/shortage.svg", copy.shortage )
                , fullImage ( "/static/images/future-of-health.svg", copy.futureOfhealth )
                , fullImage ( "/static/images/globally-hiring.svg", copy.globallyHiring )
                , h2 copy.stepsHeading
                , orderedList copy.stepsToSafegard
                , cta ( Contact, "Meet a Staffing Expert" )
                , fullImage ( "/static/images/staffing-1-issue.svg", copy.staffingIssue )
                , h2 copy.peaceOfMind
                , styledImage [ width (fill |> maximum 280), centerX ] ( "/static/images/peace-of-mind.svg", copy.peaceOfMindText )
                , meetWithExpertCta
                ]
            ]
        ]
    ]


desktopView : List (Element msg)
desktopView =
    [ column
        [ width fill
        , paddingXY 0 50
        , spacingXY 0 20
        ]
        [ column [ width fill, height fill, paddingXY 0 0, spacing 20 ]
            [ column [ width fill, spacing 50, height fill ]
                [ row [ width fill, height fill, spacing 100 ]
                    [ column [ width fill, spacing 20, alignTop ]
                        [ column [ width fill, height fill, centerY, paddingXY 0 150, spacing 30 ]
                            [ paragraph [ Font.size 30, width (fill |> maximum 500), height fill, centerY ] [ text "Discover what the global health care talent stream can really do" ]
                            , paragraph Styles.paragraph [ text "The right staff. Expertise on demand. Find everything you need to optimize your staffing needs — with Flint." ]
                            , meetWithExpertTop
                            ]
                        ]
                    , fullImage ( "/static/images/shortage.svg", copy.shortage )
                    ]
                , row [ width fill, alignTop, spacing 100 ]
                    [ fullImage ( "/static/images/globally-hiring.svg", copy.globallyHiring )
                    , fullImage ( "/static/images/future-of-health.svg", copy.futureOfhealth )
                    ]
                , meetWithExpertCta
                , row [ width fill, alignTop, spacing 100 ]
                    [ column [ width fill, alignTop ]
                        [ h2 copy.stepsHeading
                        , orderedList copy.stepsToSafegard
                        ]
                    , fullImage ( "/static/images/staffing-1-issue.svg", copy.staffingIssue )
                    ]
                , meetWithExpertCta
                , h2 copy.peaceOfMind
                , styledImage [ width (fill |> maximum 280), centerX ] ( "/static/images/peace-of-mind.svg", copy.peaceOfMindText )
                , meetWithExpertCta
                ]
            ]
        ]
    ]


meetWithExpertCta : Element msg
meetWithExpertCta =
    cta ( Contact, "Meet a Staffing Expert" )


meetWithExpertTop : Element msg
meetWithExpertTop =
    leftCta ( Contact, "Meet a Staffing Expert" )


cta : ( Page, String ) -> Element msg
cta ( where_, t ) =
    column ([ width fill, centerX ] ++ Styles.paragraph)
        [ link ([ width fill, centerX, width (maximum 200 fill), Font.center, mouseOver [ Background.color colors.blue1 ] ] ++ buttons.primary)
            { url = toPath where_, label = text t }
        ]


leftCta : ( Page, String ) -> Element msg
leftCta ( where_, t ) =
    column ([ width fill, centerX ] ++ Styles.paragraph)
        [ link ([ width fill, width (maximum 200 fill), Font.center, mouseOver [ Background.color colors.blue1 ] ] ++ buttons.primary)
            { url = toPath where_, label = text t }
        ]


orderedList : List ( String, String ) -> Element msg
orderedList list =
    column [ width fill, spacing 20, paddingXY 30 30 ] (list |> List.map (\( n, t ) -> row [ spacing 5 ] [ el [ alignTop ] (text n), paragraph [ alignTop ] [ text t ] ]))


styledImage : List (Element.Attribute msg) -> ( String, String ) -> Element msg
styledImage styles ( src, t ) =
    image styles
        { src = src
        , description = t
        }


fullImage : ( String, String ) -> Element msg
fullImage =
    styledImage [ width fill, centerX, alignTop ]


h2 : String -> Element msg
h2 t =
    paragraph
        [ centerX
        , paddingXY 10 10
        , width
            (fill
                |> maximum 300
            )
        , Font.size 23
        , Font.center
        , Font.semiBold
        ]
        [ text t ]


copy :
    { topHeading : String
    , top : String
    , shortage : String
    , futureOfhealth : String
    , globallyHiring : String
    , staffingIssue : String
    , peaceOfMind : String
    , peaceOfMindText : String
    , stepsHeading : String
    , stepsToSafegard : List ( String, String )
    }
copy =
    { topHeading =
        "Safeguard Your Staffing Needs"
    , top =
        "Staffing shortages are the norm in health care and it’s only getting worse"
    , shortage = "Staffing shortages are the norm in health care and it’s only getting worse"
    , futureOfhealth = "The future of healthcare staffing."
    , globallyHiring = "Flint makes hiring globally as easy as hiring from your own neighborhoode"
    , staffingIssue = "What if staffing shortages were not your #1 issue?"
    , peaceOfMind = "Peace of Mind"
    , peaceOfMindText = "You’ve taken care of your future staffing needs, You’ve done your duty to secure proper care for future patients, You’re supporting the global health care community."
    , stepsHeading = "Steps to Safeguard Your Staffing Needs"
    , stepsToSafegard =
        [ ( "1.", " Plan out your staffing requirments for the next 5 years." )
        , ( "2.", " Lock in the number of staff you need for a predictable future." )
        , ( "3.", " We’ll source accredited  candidates from across the globe." )
        , ( "4.", " We manage all the licensing, legal, and government hurdles so you don’t have to!" )
        , ( "5.", " You extend offers to candidates and onboard them." )
        , ( "6.", " If the person for any reason is not a good fit, you can easily select an alternative candidate." )
        ]
    }
