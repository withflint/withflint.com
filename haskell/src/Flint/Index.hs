module Flint.Index (index) where

import Lucid
import Lucid.Base (makeAttribute)
import Data.Text (Text)
import Flint.Types
import Flint.Utils (comment_)
import Flint.Blog (generateMeta)
import Text.Shakespeare.Text (st, sbt)

dataWebsiteId_ :: Text -> Attribute
dataWebsiteId_ = makeAttribute "data-website-id"

index :: Config -> Maybe Meta -> Html ()
index Config { gitVersion } meta = do
  doctypehtml_ do
    html_ do
      comment_
        [sbt|
            |This site is built in Elm.
            |You can see the source code at https://github.com/withflint/withflint.com
            |
            |If you are interested in building Elm with us,
            |tell us about your interest at join@withflint.command
            |]

      head_ do
        meta_ [ charset_ "utf-8" ]
        
        title_ "Flint - Securing Nurses for Your Future"
        
        meta_ [ name_ "viewport"
              , content_ "width=device-width, initial-scale=1.0"
              ]
        
        link_ [ href_ "/static/style.css"
              , rel_ "stylesheet"
              ]
        
        link_ [ href_ "/static/fonts.css"
              , rel_ "stylesheet"
              ]

        maybe "" generateMeta meta
      
      body_ do
        div_ [ id_ "flint" ] ""
        
        script_ [ src_ [st|/static/#{gitVersion}/elm.js|] ] ""

        script_ []
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

        script_ [ async_ ""
                , defer_ ""
                , dataWebsiteId_ "61e2287e-b2c6-44e9-8446-48339059a08c"
                , src_ "https://a.withflint.com/umami.js"
                ]
          ""

        script_ [ src_ "/static/intercom.js" ] ""
