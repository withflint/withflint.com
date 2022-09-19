{-# LANGUAGE BlockArguments #-}

module Flint.Index (index) where

import Data.Text (Text)
import Flint.Blog (generateMeta)
import Flint.Types
import Lucid
import Lucid.Base (makeAttribute)
import Text.Shakespeare.Text (lt, sbt, st)

dataWebsiteId_ :: Text -> Attribute
dataWebsiteId_ = makeAttribute "data-website-id"

index :: Config -> Maybe Meta -> Html ()
index Config {gitVersion} meta = do
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

        script_
          [ async_ ""
          , defer_ ""
          , dataWebsiteId_ "61e2287e-b2c6-44e9-8446-48339059a08c"
          , src_ "https://a.withflint.com/umami.js"
          ]
          ""

        script_
          []
          [sbt|
              |const monitor = () => Sentry.init({
              |  dsn: "https://d99039d1442643319ca4965c8e5505cf@o1259575.ingest.sentry.io/6434541",
              |  integrations: [new Sentry.BrowserTracing()],
              |  tracesSampleRate: 1.0,
              |})
              |]

        script_
          [ async_ ""
          , defer_ ""
          , onload_ "monitor()"
          , src_ "https://browser.sentry-cdn.com/7.0.0/bundle.tracing.min.js"
          , integrity_ "sha384-+zViWRWnRAkk9/+V2CRRVm1tuQEGGqye3jiEC8SDdjaOyzmv86+kvpl6NnRy9QIF"
          , crossorigin_ "anonymous"
          ]
          ""

        script_ [src_ [st|/static/#{gitVersion}/app.js|]] ""

comment_ :: Text -> Html ()
comment_ body = do
  toHtmlRaw [lt|<!-- #{body} -->|]
