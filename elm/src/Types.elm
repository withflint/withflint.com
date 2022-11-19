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
import Join.Types
import Landing.Australia.Types
import Landing.Canada.Types
import Landing.Chile.Types
import Landing.Mexico.Types
import Landing.Singapore.Types
import Partners.Types
import Partnerships.Types
import Router.Types
import Url exposing (Url)


type alias Model =
    { router : Router.Types.Model
    , aboutUs : About.Types.Model
    , home : Home.Types.Model
    , partners : Partners.Types.Model
    , join : Join.Types.Model
    , australia : Landing.Australia.Types.Model
    , mexico : Landing.Mexico.Types.Model
    , canada : Landing.Canada.Types.Model
    , chile : Landing.Chile.Types.Model
    , singapore : Landing.Singapore.Types.Model
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
      --
    | MsgForJoin Join.Types.Msg
      --
    | MsgForPartners Partners.Types.Msg
      --
    | MsgForAustralia Landing.Australia.Types.Msg
      --
    | MsgForMexico Landing.Mexico.Types.Msg
      --
    | MsgForCanada Landing.Canada.Types.Msg
      --
    | MsgForChile Landing.Chile.Types.Msg
      --
    | MsgForSingapore Landing.Singapore.Types.Msg
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
