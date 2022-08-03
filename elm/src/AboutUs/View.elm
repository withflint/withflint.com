module AboutUs.View exposing (view)

import AboutUs.Types exposing (Model)
import Device
import Element
    exposing
        ( Element
        , centerX
        , column
        , el
        , fill
        , height
        , link
        , maximum
        , minimum
        , mouseOver
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , row
        , shrink
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Layout exposing (Layout, footer)
import Styles exposing (buttons, colors, heading, wf)


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
