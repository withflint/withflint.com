module Router.Routes exposing (Page(..), parse, routes, toPath)

import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


type Page
    = Home
    | NotFound
    | Partnerships
    | About
    | JoinTheTeam String
    | NurseCareers String
    | Australia
    | Mexico
    | Canada
    | Chile
    | Singapore
    | Blog String
    | FaqNurses


routes : Parser (Page -> a) a
routes =
    oneOf
        [ map Home top
        , map NotFound (s "404")
        , map About (s "about")
        , map Partnerships (s "partnerships")
        , map (JoinTheTeam "") (s "join")
        , map JoinTheTeam (s "join" </> string)
        , map (NurseCareers "") (s "nurse-careers")
        , map Australia (s "australia")
        , map Mexico (s "mexico")
        , map Canada (s "canada")
        , map Chile (s "chile")
        , map Singapore (s "singapore")
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

        Partnerships ->
            absolute [ "partnerships" ] []

        About ->
            absolute [ "about" ] []

        JoinTheTeam id ->
            absolute [ "join", id ] []

        NurseCareers id ->
            absolute [ "nurse-careers", id ] []

        Australia ->
            absolute [ "australia" ] []

        Mexico ->
            absolute [ "mexico" ] []

        Canada ->
            absolute [ "canada" ] []

        Chile ->
            absolute [ "chile" ] []

        Singapore ->
            absolute [ "singapore" ] []

        Blog path ->
            absolute [ "blog", path ] []

        FaqNurses ->
            absolute [ "internationally-educated-nurses-faq" ] []


parse : Url -> Maybe Page
parse =
    Url.Parser.parse routes
