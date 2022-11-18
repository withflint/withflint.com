module Types exposing
    ( Model
    , Msg(..)
    )

import About.Types
import Australia.Types
import Blog.Types
import Browser.Dom exposing (Viewport)
import Canada.Types
import Chile.Types
import Device exposing (Device)
import FaqNurses.Types
import Home.Types
import Jobs.Types
import Mexico.Types
import Partnerships.Types
import Router.Types
import Url exposing (Url)


type alias Model =
    { router : Router.Types.Model
    , aboutUs : About.Types.Model
    , home : Home.Types.Model
    , jobs : Jobs.Types.Model
    , healthcare : Jobs.Types.Model
    , australia : Australia.Types.Model
    , mexico : Mexico.Types.Model
    , canada : Canada.Types.Model
    , chile : Chile.Types.Model
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
    | MsgForAustralia Australia.Types.Msg
      --
    | MsgForMexico Mexico.Types.Msg
      --
    | MsgForCanada Canada.Types.Msg
      --
    | MsgForChile Chile.Types.Msg
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
