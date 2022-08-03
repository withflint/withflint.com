module Device exposing (Device(..), Viewport, classify)


type Device
    = Phone Viewport
    | Tablet Viewport
    | Desktop Viewport
    | NotSet


type alias Viewport =
    { width : Int
    , height : Int
    }


classify : { window | height : Int, width : Int } -> Device
classify window =
    let
        viewport =
            { width = window.width
            , height = window.height
            }
    in
    -- if window.width <= 490 then
    --     Phone viewport
    -- else if window.width <= 950 then
    --     Tablet viewport
    -- else
    --     Desktop viewport
    if window.width < 700 then
        Phone viewport

    else if window.width <= 1200 then
        Tablet viewport

    else
        Desktop viewport
