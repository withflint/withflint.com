module Flint.Index (index) where

{-
import Text.Blaze.Html5 as Html
import Text.Blaze.Html5.Attributes as Attr

index :: String -> Html
index gitVersion = do
  Html.docTypeHtml do
    Html.html do
      Html.head do
        Html.meta ! Attr.charset "utf-8"
        Html.title "Flint - Safeguard Your Health Care Staffing Needs"
        Html.meta ! Attr.name "viewport" ! Attr.content "width=device-width, initial-scale=1.0"
        Html.style ! Attr.type_ "text/css" $ text "body { min-height: 100vh; }"
        Html.link ! Attr.href "/static/fonts.css" ! Attr.rel "stylesheet"


-}

import Lucid
import Lucid.Base (makeAttribute)
import Data.Text (Text)
import Flint.Types
import Flint.Blog (Meta (..), generateMeta)
import Text.Julius
import Text.Shakespeare.Text 

dataWebsiteId_ :: Text -> Attribute
dataWebsiteId_ = makeAttribute "data-website-id"

index :: Config -> Maybe Meta -> Html ()
index Config { gitVersion } meta = do
  doctypehtml_ do
    html_ do
      head_ do
        meta_ [ charset_ "utf-8" ]
        
        title_ "Flint - Safeguard Your Healthcare Staffing Needs"
        
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
          [st|const pathname =
                    window &&
                    window.location &&
                    window.location.pathname &&
                    window.location.pathname.indexOf("/blog/") > -1 &&
                    window.location.pathname.substring(window.location.pathname.lastIndexOf('/') + 1)
                    
              const init = {
                  node: document.getElementById("flint"),
                  flags: {
                      article: pathname || null,
                      gitVersion: "#{gitVersion}"
                  }
              }
              
              const app = Elm.Main.init(init)
              |]

        script_ [ async_ ""
                , defer_ ""
                , dataWebsiteId_ "61e2287e-b2c6-44e9-8446-48339059a08c"
                , src_ "https://a.withflint.com/umami.js"
                ]
          ""

        script_ [ src_ "/static/intercom.js" ] ""
