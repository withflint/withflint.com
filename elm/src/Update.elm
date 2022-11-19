module Update exposing (init, update)

import About.Update
import Blog.Types
import Blog.Update
import Browser.Dom
import Browser.Navigation exposing (Key)
import Device exposing (Device(..), classify)
import FaqNurses.Update
import Home.Update
import Html.Attributes exposing (width)
import Join.Types
import Join.Update
import Landing.Australia.Update
import Landing.Canada.Update
import Landing.Chile.Update
import Landing.Mexico.Update
import Landing.Singapore.Update
import Partners.Types
import Partners.Update
import Partnerships.Update
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

        ( join, initJoin ) =
            Join.Update.init gitVersion
                url
                key
                { endpoint = "/j"
                , page = "join"
                , apply = "/apply"
                }
                |> SubModule.init
                    { toMsg = MsgForJoin }

        ( partners, initPartners ) =
            Partners.Update.init gitVersion
                url
                key
                { endpoint = "/hc"
                , page = "nurse-careers"
                , apply = "/happly"
                }
                |> SubModule.init
                    { toMsg = MsgForPartners
                    }

        ( australia, _ ) =
            Landing.Australia.Update.init gitVersion
                url
                key
                |> SubModule.init
                    { toMsg = MsgForAustralia
                    }

        ( mexico, _ ) =
            Landing.Mexico.Update.init gitVersion
                url
                key
                |> SubModule.init
                    { toMsg = MsgForMexico
                    }

        ( canada, _ ) =
            Landing.Canada.Update.init gitVersion
                url
                key
                |> SubModule.init
                    { toMsg = MsgForCanada
                    }

        ( chile, _ ) =
            Landing.Chile.Update.init gitVersion
                url
                key
                |> SubModule.init
                    { toMsg = MsgForChile
                    }

        ( singapore, _ ) =
            Landing.Singapore.Update.init gitVersion
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
        , join = join
        , partners = partners
        , australia = australia
        , mexico = mexico
        , canada = canada
        , chile = chile
        , singapore = singapore
        , blog = blog
        , faqNurses = faqNurses
        , partnerships = Partnerships.Update.init
        , title = "Flint - Securing Nurses for Your Future"
        , device = NotSet
        , url = url
        }
        (Task.perform Load Browser.Dom.getViewport)
        |> initRouter
        |> initJoin
        |> initPartners
        |> initBlog


update : Msg -> Model -> Return.Return Msg Model
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

            MsgForJoin joinMsg ->
                Join.Update.update joinMsg model.join
                    |> SubModule.update
                        { toMsg = MsgForJoin
                        , toModel =
                            \join -> { model | join = join }
                        }

            MsgForPartners partnersMsg ->
                Partners.Update.update partnersMsg model.partners
                    |> SubModule.update
                        { toMsg = MsgForPartners
                        , toModel =
                            \partners -> { model | partners = partners }
                        }

            MsgForPartnerships partnershipsMsg ->
                Partnerships.Update.update partnershipsMsg model.partnerships
                    |> SubModule.update
                        { toMsg = MsgForPartnerships
                        , toModel =
                            \partnerships -> { model | partnerships = partnerships }
                        }

            MsgForAustralia australiaMsg ->
                Landing.Australia.Update.update australiaMsg model.australia
                    |> SubModule.update
                        { toMsg = MsgForAustralia
                        , toModel =
                            \australia -> { model | australia = australia }
                        }

            MsgForMexico mexicoMsg ->
                Landing.Mexico.Update.update mexicoMsg model.mexico
                    |> SubModule.update
                        { toMsg = MsgForMexico
                        , toModel =
                            \mexico -> { model | mexico = mexico }
                        }

            MsgForCanada canadaMsg ->
                Landing.Canada.Update.update canadaMsg model.canada
                    |> SubModule.update
                        { toMsg = MsgForCanada
                        , toModel =
                            \canada -> { model | canada = canada }
                        }

            MsgForChile chileMsg ->
                Landing.Chile.Update.update chileMsg model.chile
                    |> SubModule.update
                        { toMsg = MsgForChile
                        , toModel =
                            \chile -> { model | chile = chile }
                        }

            MsgForSingapore singaporeMsg ->
                Landing.Singapore.Update.update singaporeMsg model.singapore
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
                                Join.Update.update (Join.Types.SwitchView Join.Types.JobsView) model.join
                                    |> SubModule.update
                                        { toMsg = MsgForJoin
                                        , toModel =
                                            \join -> { model | join = resetPhoneMenuState join }
                                        }

                            Just (Router.Routes.Partners "") ->
                                Partners.Update.update (Partners.Types.SwitchView Partners.Types.JobsView) model.partners
                                    |> SubModule.update
                                        { toMsg = MsgForPartners
                                        , toModel =
                                            \partners -> { model | partners = resetPhoneMenuState partners }
                                        }

                            Just (Router.Routes.Partners jobId) ->
                                Partners.Update.update (Partners.Types.SwitchView (Partners.Types.ApplyView jobId)) model.partners
                                    |> SubModule.update
                                        { toMsg = MsgForPartners
                                        , toModel =
                                            \partners -> { model | partners = resetPhoneMenuState partners }
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
            model.join.title

        Partners _ ->
            model.partners.title

        Blog _ ->
            model.blog.title

        FaqNurses ->
            model.faqNurses.title

        About ->
            model.aboutUs.title

        _ ->
            model.home.title
