module Jobs.Copy exposing (Copy, join, nurse)

import Element exposing (Element, link, text)
import Router.Routes exposing (Page(..), toPath)
import Styles


type alias Copy msg =
    { desktopHeader : String
    , phoneHeader : String
    , paragraph1 : String
    , paragraph2 : String
    , why : String
    , title : String
    , pageTitle : String
    , other : Maybe (List (Element msg))
    }


join : Copy msg
join =
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
            ]
    }


nurse : Copy msg
nurse =
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
