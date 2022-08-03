module AboutUs.View exposing (view)

import AboutUs.Types exposing (Model)
import Device
import Element
    exposing
        ( Element
        , column
        , fill
        , height
        )
import Layout exposing (Layout)
import Styles exposing (wf)


view : Device.Device -> Model -> Layout msg
view device _ =
    { phone =
        [ column
            [ wf
            , height fill
            ]
            (desktopView device
             --++ footer.phone
            )
        ]
    , tablet =
        [ column
            [ wf
            , height fill
            ]
            (desktopView device
             --++ footer.tablet
            )
        ]
    , desktop =
        [ column
            [ wf
            , height fill
            ]
            (desktopView device
             -- ++ footer.desktop
            )
        ]
    }


desktopView : Device.Device -> List (Element msg)
desktopView device =
    [ Element.none ]
