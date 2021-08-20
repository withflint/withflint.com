module Device exposing (Device(..), classify)


type Device
    = Phone
    | Tablet
    | Desktop
    | NotSet


classify : { window | height : Int, width : Int } -> Device
classify window =
    if window.width < 700 then
        Phone

    else if window.width <= 1200 then
        Tablet

    else
        Desktop
