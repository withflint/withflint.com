module Update exposing (init, update)

import About.Update
import Australia.Update
import Blog.Types
import Blog.Update
import Browser.Dom
import Browser.Navigation exposing (Key)
import Canada.Update
import Chile.Update
import Device exposing (Device(..), classify)
import FaqNurses.Update
import Home.Update
import Html.Attributes exposing (width)
import Jobs.Copy
import Jobs.Types exposing (CurrentPage(..))
import Jobs.Update
import Mexico.Update
import NurseCareers.Update
import Partnerships.Update
import Ports
import Return exposing (Return, return, singleton)
import Router.Routes exposing (Page(..))
import Router.Types
import Router.Update
import Singapore.Update
import SubModule
import Task
import Types exposing (Model, Msg(..))
import Url exposing (Url)


init : { article : Maybe String, gitVersion : String } -> Url -> Key -> Return Msg Model
init { article, gitVersion } url key =
    let
        ( router, initRouter ) =
            Router.Update.init url key
                |> SubModule.initWithEffect
                    { toMsg = MsgForRouter
                    , effectToMsg = EffFromRouter
                    }

        ( jobs, initJobs ) =
            Jobs.Update.init gitVersion
                url
                key
                { endpoint = "/j"
                , page = "join"
                , copy = Jobs.Copy.join
                , apply = "/apply"
                , page_ = JoinTheTeamPage
                }
                |> SubModule.init
                    { toMsg = MsgForJobs
                    }

        ( nurseCareers, _ ) =
            NurseCareers.Update.init

        ( australia, _ ) =
            Australia.Update.init gitVersion
                url
                key
                |> SubModule.init
                    { toMsg = MsgForAustralia
                    }

        ( mexico, _ ) =
            Mexico.Update.init gitVersion
                url
                key
                |> SubModule.init
                    { toMsg = MsgForMexico
                    }

        ( canada, _ ) =
            Canada.Update.init gitVersion
                url
                key
                |> SubModule.init
                    { toMsg = MsgForCanada
                    }

        ( chile, _ ) =
            Chile.Update.init gitVersion
                url
                key
                |> SubModule.init
                    { toMsg = MsgForChile
                    }

        ( singapore, _ ) =
            Singapore.Update.init gitVersion
                url
                key
                |> SubModule.init
                    { toMsg = MsgForSingapore
                    }

        ( blog, initBlog ) =
            Blog.Update.init article
                |> SubModule.init
                    { toMsg = MsgForBlog
                    }

        ( faqNurses, _ ) =
            FaqNurses.Update.init
                |> SubModule.init
                    { toMsg = MsgForFaqNurses
                    }
    in
    return
        { router = router
        , home = Home.Update.init
        , aboutUs = About.Update.init
        , jobs = jobs
        , nurseCareers = nurseCareers
        , australia = australia
        , mexico = mexico
        , canada = canada
        , chile = chile
        , singapore = singapore
        , blog = blog
        , faqNurses = faqNurses
        , partnerships = Partnerships.Update.init
        , showNavMenu = False
        , title = "Flint - Securing Nurses for Your Future"
        , device = NotSet
        , url = url
        }
        (Task.perform Load Browser.Dom.getViewport)
        |> initRouter
        |> initJobs
        |> initBlog


update : Msg -> Model -> Return Msg Model
update msg model =
    let
        resetPhoneMenuState m =
            if .isPhoneMenuVisible m then
                { m | isPhoneMenuVisible = False }

            else
                m
    in
    Return.map updateTitle <|
        case msg of
            MsgForRouter routerMsg ->
                Router.Update.update routerMsg model.router
                    |> SubModule.updateWithEffect
                        { toMsg = MsgForRouter
                        , effectToMsg = EffFromRouter
                        , toModel =
                            \router -> { model | router = router, showNavMenu = False }
                        }

            MsgForHome homeMsg ->
                Home.Update.update homeMsg model.home
                    |> SubModule.update
                        { toMsg = MsgForHome
                        , toModel =
                            \home -> { model | home = home }
                        }

            MsgForJobs jobsMsg ->
                Jobs.Update.update jobsMsg model.jobs
                    |> SubModule.update
                        { toMsg = MsgForJobs
                        , toModel =
                            \jobs -> { model | jobs = jobs }
                        }

            MsgForNurseCareers nurseCareersMsg ->
                NurseCareers.Update.update nurseCareersMsg model.nurseCareers
                    |> SubModule.update
                        { toMsg = MsgForNurseCareers
                        , toModel = \nurseCareers -> { model | nurseCareers = nurseCareers }
                        }

            MsgForPartnerships partnershipsMsg ->
                Partnerships.Update.update partnershipsMsg model.partnerships
                    |> SubModule.update
                        { toMsg = MsgForPartnerships
                        , toModel =
                            \partnerships -> { model | partnerships = partnerships }
                        }

            MsgForAustralia australiaMsg ->
                Australia.Update.update australiaMsg model.australia
                    |> SubModule.update
                        { toMsg = MsgForAustralia
                        , toModel =
                            \australia -> { model | australia = australia }
                        }

            MsgForMexico mexicoMsg ->
                Mexico.Update.update mexicoMsg model.mexico
                    |> SubModule.update
                        { toMsg = MsgForMexico
                        , toModel =
                            \mexico -> { model | mexico = mexico }
                        }

            MsgForCanada canadaMsg ->
                Canada.Update.update canadaMsg model.canada
                    |> SubModule.update
                        { toMsg = MsgForCanada
                        , toModel =
                            \canada -> { model | canada = canada }
                        }

            MsgForChile chileMsg ->
                Chile.Update.update chileMsg model.chile
                    |> SubModule.update
                        { toMsg = MsgForChile
                        , toModel =
                            \chile -> { model | chile = chile }
                        }

            MsgForSingapore singaporeMsg ->
                Singapore.Update.update singaporeMsg model.singapore
                    |> SubModule.update
                        { toMsg = MsgForSingapore
                        , toModel =
                            \singapore -> { model | singapore = singapore }
                        }

            MsgForBlog blogMsg ->
                Blog.Update.update blogMsg model.blog
                    |> SubModule.update
                        { toMsg = MsgForBlog
                        , toModel =
                            \blog -> { model | blog = blog }
                        }

            MsgForFaqNurses faqNursesMsg ->
                FaqNurses.Update.update faqNursesMsg model.faqNurses
                    |> SubModule.update
                        { toMsg = MsgForFaqNurses
                        , toModel =
                            \faqNurses -> { model | faqNurses = faqNurses }
                        }

            MsgForAboutUs msgForAboutUs ->
                About.Update.update msgForAboutUs model.aboutUs
                    |> SubModule.update
                        { toMsg = MsgForAboutUs
                        , toModel =
                            \aboutUs -> { model | aboutUs = aboutUs }
                        }

            ToggleNavMenu ->
                return model (Ports.toggleNavMenu ())

            NavMenuToggled ->
                singleton { model | showNavMenu = not model.showNavMenu }

            EffFromRouter eff ->
                case eff of
                    Router.Types.UrlChange url ->
                        case Router.Routes.parse url of
                            Just (Router.Routes.Blog article) ->
                                Blog.Update.update (Blog.Types.LoadArticle article) model.blog
                                    |> SubModule.update
                                        { toMsg = MsgForBlog
                                        , toModel =
                                            \blog -> { model | blog = resetPhoneMenuState blog }
                                        }

                            Just (Router.Routes.JoinTheTeam "") ->
                                Jobs.Update.update (Jobs.Types.SwitchView Jobs.Types.JobsView) model.jobs
                                    |> SubModule.update
                                        { toMsg = MsgForJobs
                                        , toModel =
                                            \jobs -> { model | jobs = resetPhoneMenuState jobs }
                                        }

                            Just (Router.Routes.JoinTheTeam jobId) ->
                                Jobs.Update.update (Jobs.Types.SwitchView (Jobs.Types.ApplyView jobId)) model.jobs
                                    |> SubModule.update
                                        { toMsg = MsgForJobs
                                        , toModel =
                                            \jobs -> { model | jobs = resetPhoneMenuState jobs }
                                        }

                            Just Router.Routes.Australia ->
                                singleton
                                    { model
                                        | australia = model.australia |> resetPhoneMenuState
                                    }

                            Just Router.Routes.Mexico ->
                                singleton
                                    { model
                                        | mexico = model.mexico |> resetPhoneMenuState
                                    }

                            Just Router.Routes.Canada ->
                                singleton
                                    { model
                                        | canada = model.canada |> resetPhoneMenuState
                                    }

                            Just Router.Routes.Chile ->
                                singleton
                                    { model
                                        | chile = model.chile |> resetPhoneMenuState
                                    }

                            Just Router.Routes.Singapore ->
                                singleton
                                    { model
                                        | singapore = model.singapore |> resetPhoneMenuState
                                    }

                            Just Router.Routes.Partnerships ->
                                singleton
                                    { model
                                        | partnerships = model.partnerships |> resetPhoneMenuState
                                    }

                            Just Router.Routes.FaqNurses ->
                                singleton
                                    { model
                                        | faqNurses = model.faqNurses |> resetPhoneMenuState
                                    }

                            Just Router.Routes.About ->
                                singleton
                                    { model
                                        | aboutUs = model.aboutUs |> resetPhoneMenuState
                                    }

                            _ ->
                                singleton model

            Load viewport ->
                singleton
                    { model
                        | device =
                            classify
                                { width = round viewport.viewport.width
                                , height = round viewport.viewport.height
                                }
                    }

            Resize width height ->
                singleton
                    { model
                        | device =
                            classify
                                { width = width
                                , height = height
                                }
                    }


updateTitle : Model -> Model
updateTitle model =
    { model | title = pageTitle model }


pageTitle : Model -> String
pageTitle model =
    case model.router.page of
        NotFound ->
            "404 Not Found - Flint"

        JoinTheTeam _ ->
            model.jobs.title

        NurseCareers _ ->
            "Nurse Success - Flint"

        Blog _ ->
            model.blog.title

        FaqNurses ->
            model.faqNurses.title

        About ->
            model.aboutUs.title

        _ ->
            model.home.title
