module Jobs.Copy exposing (Copy, join)

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
