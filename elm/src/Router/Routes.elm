module Router.Routes exposing (Page(..), parse, routes, toPath)

import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


type Page
    = Home
    | NotFound
    | Contact
    | Jobs String
    | HealthCare String
    | Blog String
    | FaqNurses


routes : Parser (Page -> a) a
routes =
    oneOf
        [ map Home top
        , map NotFound (s "404")
        , map Contact (s "contact")
        , map (Jobs "") (s "jobs")
        , map Jobs (s "jobs" </> string)
        , map (HealthCare "") (s "health-care-jobs")
        , map HealthCare (s "health-care-jobs" </> string)
        , map (Blog "") (s "blog")
        , map Blog (s "blog" </> string)
        , map FaqNurses (s "internationally-educated-nurses-faq")
        ]


toPath : Page -> String
toPath page =
    case page of
        Home ->
            "/"

        NotFound ->
            absolute [ "404" ] []

        Contact ->
            absolute [ "contact" ] []

        Jobs jobId ->
            absolute [ "jobs", jobId ] []

        HealthCare jobId ->
            absolute [ "health-care-jobs", jobId ] []

        Blog path ->
            absolute [ "blog", path ] []

        FaqNurses ->
            absolute [ "internationally-educated-nurses-faq" ] []


parse : Url -> Maybe Page
parse =
    Url.Parser.parse routes
