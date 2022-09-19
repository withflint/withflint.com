module Update exposing (init, update)

import AboutUs.Update
import Blog.Types
import Blog.Update
import Browser.Dom
import Browser.Navigation exposing (Key)
import Device exposing (Device(..), classify)
import FaqNurses.Update
import Home.Update
import Html.Attributes exposing (width)
import Jobs.Types exposing (Config, CurrentPage(..))
import Jobs.Update
import Partnerships.Update
import Return exposing (Return, return, singleton)
import Router.Routes exposing (Page(..))
import Router.Types
import Router.Update
import SubModule
import Task
import Types exposing (Model, Msg(..))
import Url exposing (Url)
import View exposing (joinCopy, nurseCareersCopy)


flintConfig : Config
flintConfig =
    { endpoint = "/j"
    , page = "join"
    , copy = joinCopy
    , apply = "/apply"
    , page_ = JoinTheTeamPage
    }


healthCareConfig : Config
healthCareConfig =
    { endpoint = "/hc"
    , page = "nurse-careers"
    , copy = nurseCareersCopy
    , apply = "/happly"
    , page_ = NurseCareersPage
    }


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
            Jobs.Update.init gitVersion url key flintConfig
                |> SubModule.init
                    { toMsg = MsgForJobs
                    }

        ( healthCare, initHealthCare ) =
            Jobs.Update.init gitVersion url key healthCareConfig
                |> SubModule.init
                    { toMsg = MsgForHealthCare
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
        , aboutUs = AboutUs.Update.init
        , jobs = jobs
        , healthCare = healthCare
        , blog = blog
        , faqNurses = faqNurses
        , partnerships = Partnerships.Update.init
        , title = "Flint - Securing Nurses for Your Future"
        , device = NotSet
        , url = url
        }
        (Task.perform Load Browser.Dom.getViewport)
        |> initRouter
        |> initJobs
        |> initHealthCare
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
                            \router -> { model | router = router }
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

            MsgForHealthCare healthCareMsg ->
                Jobs.Update.update healthCareMsg model.healthCare
                    |> SubModule.update
                        { toMsg = MsgForHealthCare
                        , toModel =
                            \healthCare -> { model | healthCare = healthCare }
                        }

            MsgForPartnerships partnershipsMsg ->
                Partnerships.Update.update partnershipsMsg model.partnerships
                    |> SubModule.update
                        { toMsg = MsgForPartnerships
                        , toModel =
                            \partnerships -> { model | partnerships = partnerships }
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
                AboutUs.Update.update msgForAboutUs model.aboutUs
                    |> SubModule.update
                        { toMsg = MsgForAboutUs
                        , toModel =
                            \aboutUs -> { model | aboutUs = aboutUs }
                        }

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

                            Just (Router.Routes.NurseCareers "") ->
                                Jobs.Update.update (Jobs.Types.SwitchView Jobs.Types.JobsView) model.healthCare
                                    |> SubModule.update
                                        { toMsg = MsgForHealthCare
                                        , toModel =
                                            \healthCare -> { model | healthCare = resetPhoneMenuState healthCare }
                                        }

                            Just (Router.Routes.NurseCareers jobId) ->
                                Jobs.Update.update (Jobs.Types.SwitchView (Jobs.Types.ApplyView jobId)) model.healthCare
                                    |> SubModule.update
                                        { toMsg = MsgForHealthCare
                                        , toModel =
                                            \healthCare -> { model | healthCare = resetPhoneMenuState healthCare }
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

                            Just Router.Routes.AboutUs ->
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
            model.healthCare.title

        Blog _ ->
            model.blog.title

        FaqNurses ->
            model.faqNurses.title

        AboutUs ->
            model.aboutUs.title

        _ ->
            model.home.title
