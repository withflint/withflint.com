module Types exposing
    ( Model
    , Msg(..)
    )

import About.Types
import Blog.Types
import Browser.Dom exposing (Viewport)
import Device exposing (Device)
import FaqNurses.Types
import Home.Types
import Jobs.Types
import Partnerships.Types
import Router.Types
import Url exposing (Url)


type alias Model =
    { router : Router.Types.Model
    , aboutUs : About.Types.Model
    , home : Home.Types.Model
    , jobs : Jobs.Types.Model
    , healthcare : Jobs.Types.Model
    , blog : Blog.Types.Model
    , faqNurses : FaqNurses.Types.Model
    , partnerships : Partnerships.Types.Model
    , title : String
    , device : Device
    , url : Url
    }


type Msg
    = MsgForRouter Router.Types.Msg
    | EffFromRouter Router.Types.Eff
      --
    | MsgForHome Home.Types.Msg
    | MsgForJobs Jobs.Types.Msg
      --
    | MsgForHealthCare Jobs.Types.Msg
      --
    | MsgForBlog Blog.Types.Msg
      --
    | MsgForFaqNurses FaqNurses.Types.Msg
      --
    | MsgForPartnerships Partnerships.Types.Msg
      --
    | MsgForAboutUs About.Types.Msg
    | Load Viewport
    | Resize Int Int
