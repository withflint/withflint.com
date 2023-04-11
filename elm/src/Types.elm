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
import NurseCareers.Types
import Partnerships.Types
import Router.Types
import Singapore.Types
import Url exposing (Url)


type alias Model =
    { router : Router.Types.Model
    , aboutUs : About.Types.Model
    , home : Home.Types.Model
    , jobs : Jobs.Types.Model
    , nurseCareers : NurseCareers.Types.Model
    , australia : Australia.Types.Model
    , mexico : Mexico.Types.Model
    , canada : Canada.Types.Model
    , chile : Chile.Types.Model
    , singapore : Singapore.Types.Model
    , blog : Blog.Types.Model
    , faqNurses : FaqNurses.Types.Model
    , partnerships : Partnerships.Types.Model
    , showNavMenu : Bool
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
    | MsgForNurseCareers NurseCareers.Types.Msg
      --
    | MsgForAustralia Australia.Types.Msg
      --
    | MsgForMexico Mexico.Types.Msg
      --
    | MsgForCanada Canada.Types.Msg
      --
    | MsgForChile Chile.Types.Msg
      --
    | MsgForSingapore Singapore.Types.Msg
      --
    | MsgForBlog Blog.Types.Msg
      --
    | MsgForFaqNurses FaqNurses.Types.Msg
      --
    | MsgForPartnerships Partnerships.Types.Msg
      --
    | MsgForAboutUs About.Types.Msg
      --
    | ToggleNavMenu
    | NavMenuToggled
      --
    | Load Viewport
    | Resize Int Int
