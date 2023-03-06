{-# LANGUAGE BlockArguments #-}

module Flint.Index (index) where

import Data.Text (Text)
import Flint.Blog (generateMeta)
import Flint.Types
import Lucid (
  Attribute
  , Html
  , async_
  , body_
  , charset_
  , content_
  , crossorigin_
  , defer_
  , div_
  , doctypehtml_
  , head_
  , href_
  , html_
  , id_
  , integrity_
  , link_
  , meta_
  , name_
  , onload_
  , rel_
  , script_
  , sizes_
  , src_
  , title_
  , toHtmlRaw
  , type_
 )
import Lucid.Base (makeAttribute)
import Text.Shakespeare.Text (lt, sbt, st)

dataWebsiteId_ :: Text -> Attribute
dataWebsiteId_ = makeAttribute "data-website-id"

color_ :: Text -> Attribute
color_ = makeAttribute "color"

dataCache_ :: Text -> Attribute
dataCache_ = makeAttribute "data-cached"

index :: Config -> Maybe Meta -> Html ()
index Config {gitVersion, environment} meta = do
  doctypehtml_ do
    html_ do
      comment_
        [sbt|
            |This site is built in Elm.
            |You can see the source code at https://github.com/withflint/withflint.com
            |
            |If you are interested in building Elm with us,
            |tell us about your interest at join@withflint.com
            |]

      head_ do
        meta_ [charset_ "utf-8"]

        title_ "Flint - Securing Nurses for Your Future"

        meta_
          [ name_ "viewport"
          , content_ "width=device-width, initial-scale=1.0"
          ]

        link_
          [ href_ "/static/style.css?v=1"
          , rel_ "stylesheet"
          ]

        link_
          [ href_ "/static/fonts.css?v=1"
          , rel_ "stylesheet"
          ]

        link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "/static/apple-touch-icon.png"]

        link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/static/favicon-32x32.png"]

        link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/static/favicon-16x16.png"]

        link_ [rel_ "manifest", href_ "/static/site.webmanifest"]

        link_ [rel_ "mask-icon", href_ "/static/safari-pinned-tab.svg", color_ "#44376d"]

        meta_ [name_ "theme-color", content_ "#44376d"]

        maybe "" generateMeta meta

      body_ do
        div_ [id_ "flint"] ""

        script_ [src_ [st|/static/#{gitVersion}/elm.js|]] ""

        script_
          []
          [sbt|const pathname =
              |      window &&
              |      window.location &&
              |      window.location.pathname &&
              |      window.location.pathname.indexOf("/blog/") > -1 &&
              |      window.location.pathname.substring(window.location.pathname.lastIndexOf('/') + 1)
              |
              |const init = {
              |    node: document.getElementById("flint"),
              |    flags: {
              |        article: pathname || null,
              |        gitVersion: "#{gitVersion}"
              |    }
              |}
              |
              |const app = Elm.Main.init(init)
              |]

        script_ umami ""

        script_
          []
          [sbt|
              |const monitor = () => Sentry.init({
              |  dsn: "https://d99039d1442643319ca4965c8e5505cf@o1259575.ingest.sentry.io/6434541",
              |  integrations: [new Sentry.BrowserTracing()],
              |  tracesSampleRate: 1.0,
              |})
              |]

        script_ sentry ""

        script_ [src_ [st|/static/#{gitVersion}/app.js|]] ""

        script_
          google
          ""

        script_
          []
          [sbt|
              |window.dataLayer = window.dataLayer || [];
              |function gtag(){dataLayer.push(arguments);}
              |gtag('js', new Date());
              |gtag('config', 'G-DV4LVWCB0Q');
              |]

        script_
          []
          facebook

        script_ umami [sbt||]
 where
  umami =
    if environment == "production"
      then
        [ async_ ""
        , defer_ ""
        , dataWebsiteId_ "61e2287e-b2c6-44e9-8446-48339059a08c"
        , src_ "https://a.withflint.com/umami.js"
        ]
      else []
  sentry =
    if environment == "production"
      then
        [ async_ ""
        , defer_ ""
        , onload_ "monitor()"
        , src_ "https://browser.sentry-cdn.com/7.0.0/bundle.tracing.min.js"
        , integrity_ "sha384-+zViWRWnRAkk9/+V2CRRVm1tuQEGGqye3jiEC8SDdjaOyzmv86+kvpl6NnRy9QIF"
        , crossorigin_ "anonymous"
        ]
      else []
  google =
    if environment == "production"
      then
        [ async_ ""
        , defer_ ""
        , src_ "https://www.googletagmanager.com/gtag/js?id=G-DV4LVWCB0Q"
        ]
      else []
  facebook =
    if environment == "production"
      then
        [sbt|
              |!function(f,b,e,v,n,t,s)
              |{if(f.fbq)return;n=f.fbq=function(){n.callMethod?
              |n.callMethod.apply(n,arguments):n.queue.push(arguments)};
              |if(!f._fbq)f._fbq=n;n.push=n;n.loaded=!0;n.version='2.0';
              |n.queue=[];t=b.createElement(e);t.async=!0;
              |t.src=v;s=b.getElementsByTagName(e)[0];
              |s.parentNode.insertBefore(t,s)}(window, document,'script',
              |'https://connect.facebook.net/en_US/fbevents.js');
              |fbq('init', '3328443680740689');
              |fbq('track', 'PageView');
              |]
      else [sbt||]

comment_ :: Text -> Html ()
comment_ body = do
  toHtmlRaw [lt|<!-- #{body} -->|]
