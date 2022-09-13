module Styles exposing (btnFilled, btnOutline, codeFont, colors, css, font, headFont, headerGradientBackground, heading, hf, lineHeight, link, maxW, menu, minH, minW, paddingE, palette, paragraph, pb, pl, pt, title, wf, wp)

import Element
    exposing
        ( Attribute
        , Color
        , alignLeft
        , fill
        , fillPortion
        , height
        , htmlAttribute
        , maximum
        , minimum
        , mouseOver
        , paddingEach
        , paddingXY
        , rgb255
        , spacing
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes


colors :
    { white1 : Color
    , white : Color
    , gray : Color
    , gray2 : Color
    , black : Color
    , primary : Color
    , gray3 : Color
    , cremeDark : Color
    , cremeLight : Color
    , carminePink : Color
    , skyBlue : Color
    }
colors =
    { white1 = rgb255 223 223 223
    , white = rgb255 255 255 255
    , gray = rgb255 90 90 90
    , gray2 = rgb255 130 130 130
    , black = rgb255 29 30 35
    , cremeDark = rgb255 249 241 237
    , cremeLight = rgb255 255 251 248
    , carminePink = rgb255 229 72 72
    , skyBlue = rgb255 167 200 249
    , gray3 = rgb255 87 87 87
    , primary = rgb255 68 55 109
    }


palette :
    { primary : Color
    , red : Color
    , beige : Color
    , skyBlue : Color
    , cremeLight : Color
    , cremeLighter : Color
    }
palette =
    { primary = rgb255 68 55 109
    , red = rgb255 255 96 96
    , beige = rgb255 252 229 217
    , skyBlue = rgb255 218 233 255
    , cremeLight = rgb255 255 251 248
    , cremeLighter = rgb255 249 242 228
    }


headerGradientBackground : List (Attribute msg)
headerGradientBackground =
    [ htmlAttribute <| Html.Attributes.style "background" "rgb(68,55,109)"
    , htmlAttribute <| Html.Attributes.style "background" "linear-gradient(160deg, rgba(167,200,249,1) 0%, rgba(148,172,223,1) 4%, rgba(118,128,180,1) 8%, rgba(99,99,153,1) 12%, rgba(77,67,121,1) 20%, rgba(68,55,109,1) 30%, rgba(68,55,109,1) 60%, rgba(86,57,105,1) 79%, rgba(113,60,99,1) 87%, rgba(140,63,93,1) 90%, rgba(176,67,85,1) 95%, rgba(229,72,72,1) 100%)"
    ]


heading : List (Attribute msg)
heading =
    [ Font.size 40
    , spacing 10
    , paddingXY 0 10
    , headFont
    ]


paragraph : List (Attribute msg)
paragraph =
    [ alignLeft
    , spacing 10
    , Font.size 16
    , Font.color colors.gray3
    , Font.letterSpacing 1
    , Font.regular
    , width fill
    , font
    ]


font : Attribute msg
font =
    Font.family
        [ Font.typeface "Inter"
        , Font.sansSerif
        ]


headFont : Attribute msg
headFont =
    Font.family
        [ Font.typeface "Inter"
        , Font.sansSerif
        ]


codeFont : Attribute msg
codeFont =
    Font.family
        [ Font.typeface "Source Code Pro"
        , Font.sansSerif
        ]


link : List (Attribute msg)
link =
    [ Font.underline
    , mouseOver [ Font.color colors.carminePink ]
    , Font.semiBold
    , Font.color colors.gray3
    ]


title : List (Attribute msg)
title =
    [ Font.family [ Font.typeface "Inter" ]
    , Font.color colors.white
    ]


menu : List (Attribute msg)
menu =
    [ Font.color colors.white
    , Font.letterSpacing 2
    , Font.size 14
    , Font.family [ Font.typeface "Inter" ]
    , paddingEach { top = 6, bottom = 6, left = 8, right = 8 }
    , Border.rounded 6
    , Font.semiBold
    , htmlAttribute <| Html.Attributes.class "menu"
    ]


btnOutline : List (Attribute msg)
btnOutline =
    [ paddingEach { top = 10, right = 19, bottom = 10, left = 22 }
    , Border.rounded 8
    , Border.width 1
    , Border.color colors.primary
    , Font.color colors.primary
    , Font.regular
    , Font.size 16
    , mouseOver
        [ Font.color colors.cremeLight
        , Background.color colors.carminePink
        , Border.color colors.carminePink
        ]
    ]


btnFilled : { fontColor : Color, bgColor : Color } -> List (Attribute msg)
btnFilled { fontColor, bgColor } =
    [ Border.rounded 8
    , Font.color fontColor
    , Font.size 16
    , Background.color bgColor
    , paddingEach { top = 10, right = 19, bottom = 10, left = 22 }
    , Font.regular
    , mouseOver
        [ Font.color colors.cremeLight
        , Background.color colors.carminePink
        , Border.color colors.carminePink
        ]
    ]


paddingE : Int -> Int -> Int -> Int -> Attribute msg
paddingE top right bottom left =
    paddingEach { top = top, right = right, bottom = bottom, left = left }


pt : Int -> Attribute msg
pt size =
    paddingEach { top = size, left = 0, right = 0, bottom = 0 }


pl : Int -> Attribute msg
pl size =
    paddingEach { top = 0, left = size, right = size, bottom = 0 }


pb : Int -> Attribute msg
pb size =
    paddingEach { top = 0, left = 0, right = 0, bottom = size }


wf : Element.Attribute msg
wf =
    width fill


hf : Element.Attribute msg
hf =
    height fill


wp : Int -> Element.Attribute msg
wp size =
    width <| fillPortion size


minW : Int -> Attribute msg
minW size =
    width (fill |> minimum size)


maxW : Int -> Attribute msg
maxW size =
    width (fill |> maximum size)


minH : Int -> Attribute msg
minH size =
    height (fill |> minimum size)


lineHeight : Float -> Attribute msg
lineHeight size =
    css "line-height" (String.fromFloat size)


css : String -> String -> Attribute msg
css property value =
    htmlAttribute <| Html.Attributes.style property value
