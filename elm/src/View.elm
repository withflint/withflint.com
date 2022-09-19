module View exposing (joinCopy, nurseCareersCopy, view)

import AboutUs.View
import Blog.View
import Element
    exposing
        ( Element
        , alignLeft
        , centerY
        , column
        , el
        , fill
        , height
        , link
        , padding
        , paragraph
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Font as Font
import FaqNurses.View
import Home.View
import Html exposing (Html)
import Jobs.Types exposing (Copy)
import Jobs.View
import Layout exposing (layout)
import Partnerships.View
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, headerGradientBackground, pt)
import Types exposing (Model, Msg(..))


view : Model -> { title : String, body : List (Html Types.Msg) }
view model =
    { title = model.title
    , body =
        [ Element.layout (Styles.paragraph ++ [ Styles.font, width fill, height fill ]) <|
            el [ width fill, height fill ] (renderRoute model)
        ]
    }


renderRoute : Types.Model -> Element Types.Msg
renderRoute model =
    case model.router.page of
        Home ->
            Element.map MsgForHome <| layout model.device <| Home.View.view model.home model.device

        AboutUs ->
            Element.map MsgForAboutUs <| layout model.device <| AboutUs.View.view model.device model.aboutUs

        Partnerships ->
            Element.map MsgForPartnerships <| layout model.device <| Partnerships.View.view model.device model.partnerships

        JoinTheTeam _ ->
            Element.map MsgForJobs <| layout model.device <| Jobs.View.view model.device model.jobs

        NurseCareers _ ->
            Element.map MsgForHealthCare <| layout model.device <| Jobs.View.view model.device model.healthCare

        Blog _ ->
            Element.map MsgForBlog <| layout model.device <| Blog.View.view model.device model.blog

        FaqNurses ->
            Element.map MsgForFaqNurses <| layout model.device <| FaqNurses.View.view model.device model.faqNurses

        NotFound ->
            notFound


notFound : Element msg
notFound =
    column
        ([ width fill
         , height fill
         , padding 20
         , Font.color colors.cremeLight
         , spacing 20
         ]
            ++ headerGradientBackground
        )
        [ row []
            [ link []
                { url = toPath Home
                , label =
                    Element.image
                        [ centerY
                        , alignLeft
                        , width (px 100)
                        , height (px 50)
                        ]
                        { src = "/static/images/logo-white.svg?new"
                        , description = "Flint"
                        }
                }
            ]
        , row
            [ Font.size 36
            , Font.color colors.cremeLight
            , pt 32
            ]
            [ paragraph []
                [ text "404: Oops! We could not find that page. "
                ]
            ]
        , row
            [ Font.size 20
            ]
            [ paragraph []
                [ link
                    (Styles.link
                        ++ [ Font.color colors.cremeLight
                           ]
                    )
                    { url = toPath Home
                    , label = text "Go home!"
                    }
                ]
            ]
        ]


nurseCareersCopy : Copy
nurseCareersCopy =
    { desktopHeader = "We work with the very best. Quality candidates lead to quality health outcomes."
    , phoneHeader = "We work with the very best."
    , paragraph1 = "At Flint, we're committed to finding the best people to staff health care teams. We work with highly internationally educated health care professionals who display care for their patients, have quality communication skills, good empathy skills, are attentive to details, can solve problems, and display autonomy and compliances with the standards can think critically and improve the American healthcare system."
    , paragraph2 = "We work with internationally educated health care workers from around the world for staffing opportunities in the United States of America. We offer an all-inclusive solution for the workers to have a seamless transition into their new life in America. Flint offers fully sponsored licensing, immigration and relocation programs. We pay for legal and processing fees, licensing and offer premium placement."
    , why = "Why do you want to work in the United States of America?"
    , title = "Launch your nursing career in America"
    , pageTitle = "Nurse Success  - Flint"
    , other =
        Just
            [ text " "
            , link Styles.link
                { url = toPath FaqNurses
                , label = text "Learn more"
                }
            ]
    }


joinCopy : Copy
joinCopy =
    { desktopHeader = "We work with the very best."
    , phoneHeader = "We work with the very best."
    , paragraph1 = "At Flint, we're committed to hiring the best people to build our teams. Building great products takes smart, disciplined, and empathetic individuals who can understand what job the products need to get done and imagine innovative ways to achieve it. Thus we designed the hiring process to help us identify those people."
    , paragraph2 = "We foster a culture of respect, dialogue and growth where our team members can engage in a continuous conversation about product, engineering, and learning."
    , why = "Why do you want to work at Flint?"
    , title = "Join the Team"
    , pageTitle = "Join the Team - Flint"
    , other =
        Just
            [ text " "
            , link Styles.link
                { url = toPath (Blog "culture")
                , label = text "Read more about our values and culture. "
                }
            , text " "
            , text "We interview and make hires within a week from our first meet–it's a commitment."
            ]
    }
