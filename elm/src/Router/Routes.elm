module Router.Routes exposing (Page(..), parse, routes, toPath)

import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


type Page
    = Home
    | NotFound
    | Contact
    | Partnerships
    | AboutUs
    | JoinTheTeam String
    | NurseCareers String
    | Blog String
    | FaqNurses


routes : Parser (Page -> a) a
routes =
    oneOf
        [ map Home top
        , map NotFound (s "404")

        -- , map Contact (s "contact")
        , map AboutUs (s "about-us")
        , map Partnerships (s "partnerships")
        , map (JoinTheTeam "") (s "join")
        , map JoinTheTeam (s "join" </> string)
        , map (NurseCareers "") (s "nurse-careers")
        , map NurseCareers (s "nurse-careers" </> string)
        , map (Blog "") (s "blog")
        , map Blog (s "blog" </> string)
        , map FaqNurses (s "internationally-educated-nurses-faq")
        ]


toPath : Page -> String
toPath page =
    case page of
        Home ->
            absolute [] []

        NotFound ->
            absolute [ "404" ] []

        Contact ->
            absolute [ "contact" ] []

        Partnerships ->
            absolute [ "partnerships" ] []

        AboutUs ->
            absolute [ "about-us" ] []

        JoinTheTeam id ->
            absolute [ "join", id ] []

        NurseCareers id ->
            absolute [ "nurse-careers", id ] []

        Blog path ->
            absolute [ "blog", path ] []

        FaqNurses ->
            absolute [ "internationally-educated-nurses-faq" ] []


parse : Url -> Maybe Page
parse =
    Url.Parser.parse routes
