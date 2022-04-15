module Update exposing (init, update)

import Blog.Types
import Blog.Update
import Browser.Dom
import Browser.Navigation exposing (Key)
import Contact.Update
import Faq.Update
import Device exposing (Device(..), classify)
import Home.Update
import Html.Attributes exposing (width)
import Jobs.Types
import Jobs.Update
import Return exposing (Return, return, singleton)
import Router.Routes exposing (Page(..))
import Router.Types
import Router.Update
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
            Jobs.Update.init gitVersion url key Jobs.Update.flintConfig
                |> SubModule.init
                    { toMsg = MsgForJobs
                    }

        ( healthCare, initHealthCare ) =
            Jobs.Update.init gitVersion url key Jobs.Update.healthCareConfig
                |> SubModule.init
                    { toMsg = MsgForHealthCare
                    }

        ( blog, initBlog ) =
            Blog.Update.init article
                |> SubModule.init
                    { toMsg = MsgForBlog
                    }
    in
    return
        { router = router
        , contact = Contact.Update.init
        , faq = Faq.Update.init
        , home = Home.Update.init
        , jobs = jobs
        , healthCare = healthCare
        , blog = blog
        , title = "Flint - Safeguard Your Health Care Staffing Needs"
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

            MsgForJobs jobsMsg ->
                Jobs.Update.update jobsMsg model.jobs
                    |> SubModule.update
                        { toMsg = MsgForJobs
                        , toModel =
                            \jobs -> { model | jobs = jobs }
                        }

            MsgForFaq faqMsg ->
                Faq.Update.update faqMsg model.faq
                    |> SubModule.update
                        { toModel = 
                            \faq -> { model | faq = faq }
                        , toMsg = MsgForFaq
                        }

            MsgForHealthCare healthCareMsg ->
                Jobs.Update.update healthCareMsg model.healthCare
                    |> SubModule.update
                        { toMsg = MsgForHealthCare
                        , toModel =
                            \healthCare -> { model | healthCare = healthCare }
                        }

            MsgForBlog blogMsg ->
                Blog.Update.update blogMsg model.blog
                    |> SubModule.update
                        { toMsg = MsgForBlog
                        , toModel =
                            \blog -> { model | blog = blog }
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
                                            \blog -> { model | blog = blog }
                                        }

                            Just (Router.Routes.Jobs jobId) ->
                                Jobs.Update.update (Jobs.Types.SwitchView (Jobs.Types.ApplyView jobId)) model.jobs
                                    |> SubModule.update
                                        { toMsg = MsgForJobs
                                        , toModel =
                                            \jobs -> { model | jobs = jobs }
                                        }

                            Just (Router.Routes.HealthCare jobId) ->
                                Jobs.Update.update (Jobs.Types.SwitchView (Jobs.Types.ApplyView jobId)) model.healthCare
                                    |> SubModule.update
                                        { toMsg = MsgForHealthCare
                                        , toModel =
                                            \healthCare -> { model | healthCare = healthCare }
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

        Contact ->
            model.contact.title
        
        Faq ->
            model.faq.title

        Jobs _ ->
            model.jobs.title

        HealthCare _ ->
            model.healthCare.title

        Blog _ ->
            model.blog.title

        _ ->
            model.home.title
