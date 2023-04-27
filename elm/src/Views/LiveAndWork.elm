module Views.LiveAndWork exposing (view)

import Element
    exposing
        ( Element
        , centerX
        , column
        , fill
        , maximum
        , padding
        , paddingXY
        , paragraph
        , row
        , spacingXY
        , text
        , width
        , wrappedRow
        )
import Element.Font as Font
import Styles exposing (colors, wf)


view : Element msg
view =
    let
        titleStyle =
            [ Font.center
            , Font.size 24
            , Font.semiBold
            , Font.color colors.primary
            , padding 10
            ]
    in
    row [ width (fill |> maximum 800), paddingXY 12 56, centerX ]
        [ column [ wf, spacingXY 0 48 ]
            [ paragraph titleStyle [ text "US states where you can live and work" ]
            , wrappedRow
                [ wf
                , Font.color colors.primary
                , spacingXY 12 32
                ]
                [ column [ centerX, wf, spacingXY 0 22 ]
                    [ paragraph [ Font.center ] [ text "Colorado" ]
                    , paragraph [ Font.center ] [ text "Georgia" ]
                    , paragraph [ Font.center ] [ text "New Mexico" ]
                    , paragraph [ Font.center ] [ text "New Jersey" ]
                    ]
                , column [ centerX, wf, spacingXY 0 22 ]
                    [ paragraph [ Font.center ] [ text "New York" ]
                    , paragraph [ Font.center ] [ text "Tennessee" ]
                    , paragraph [ Font.center ] [ text "Texas" ]
                    , paragraph [ Font.center ] [ text "Washington" ]
                    ]
                ]
            ]
        ]
