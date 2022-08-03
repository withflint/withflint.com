module Styles exposing (ButtonStyle, btn, buttons, codeFont, colors, css, debug, font, headFont, headerGradientBackground, heading, hf, lineHeight, link, menu, minimumWidth, palette, paragraph, pb, pl, pr, pt, title, wf)

import Element
    exposing
        ( Attribute
        , Color
        , alignLeft
        , fill
        , height
        , htmlAttribute
        , minimum
        , mouseOver
        , padding
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
    { deepBlue1 : Color
    , deepBlue2 : Color
    , deepBlue3 : Color
    , blue2 : Color
    , blue3 : Color
    , green1 : Color
    , green2 : Color
    , green3 : Color
    , red1 : Color
    , red2 : Color
    , red3 : Color
    , red4 : Color
    , white1 : Color
    , white2 : Color
    , white3 : Color
    , gray1 : Color
    , gray2 : Color
    , gray3 : Color
    , black1 : Color
    , black2 : Color
    , black3 : Color
    , creme1 : Color
    , creme2 : Color
    , blue1 : Color
    , gray4 : Color
    , cremeDark : Color
    , cremeLight : Color
    , carminePink : Color
    , skyBlue : Color
    }
colors =
    { deepBlue1 = rgb255 1 55 89
    , deepBlue2 = rgb255 9 68 105
    , deepBlue3 = rgb255 11 87 137
    , blue2 = rgb255 24 147 189
    , blue3 = rgb255 29 170 217
    , green1 = rgb255 71 159 108
    , green2 = rgb255 88 190 129
    , green3 = rgb255 99 215 145
    , red1 = rgb255 106 38 41
    , red2 = rgb255 146 54 55
    , red3 = rgb255 195 58 66
    , red4 = rgb255 200 0 0
    , white1 = rgb255 223 223 223
    , white2 = rgb255 248 248 248
    , white3 = rgb255 255 255 255
    , gray1 = rgb255 90 90 90
    , gray2 = rgb255 130 130 130
    , gray3 = rgb255 204 204 204
    , black1 = rgb255 29 30 35
    , black2 = rgb255 45 45 45
    , black3 = rgb255 51 51 50
    , creme1 = rgb255 251 247 210
    , creme2 = rgb255 253 252 234
    , cremeDark = rgb255 249 241 237
    , cremeLight = rgb255 255 251 248

    -- carminePink for hover
    , carminePink = rgb255 229 72 72

    -- skyBlue for default button bg
    , skyBlue = rgb255 167 200 249

    -- , blue1 = rgb255 181 217 255
    -- text colors (blue1 for title/heading/subheading/btnText, bodyText gray4)
    , blue1 = rgb255 68 55 109
    , gray4 = rgb255 87 87 87
    }


palette :
    { primary : Color
    , red : Color
    , beige : Color
    , skyBlue : Color
    , white : Color
    , cremeLight : Color
    , cremeLighter : Color
    }
palette =
    { primary = rgb255 68 55 109
    , red = rgb255 255 96 96
    , beige = rgb255 252 229 217
    , skyBlue = rgb255 218 233 255
    , white = rgb255 255 255 255
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
    , Font.color colors.gray4
    , Font.letterSpacing 1
    , Font.regular
    , width fill
    , font
    ]


font : Attribute msg
font =
    Font.family
        [ --Font.typeface "Open Sans"
          --   Font.typeface "Lora"
          Font.typeface "Inter"
        , Font.sansSerif
        ]


headFont : Attribute msg
headFont =
    Font.family
        [ --Font.typeface "Roboto"
          --   Font.typeface "Poppins"
          Font.typeface "Inter"
        , Font.sansSerif
        ]


codeFont : Attribute msg
codeFont =
    Font.family
        [ Font.typeface "Source Code Pro"
        , Font.sansSerif
        ]


type alias ButtonStyle msg =
    { primary : List (Attribute msg)
    , secondary : List (Attribute msg)
    }


buttons : ButtonStyle msg
buttons =
    { primary =
        base
            ++ [ Background.color colors.skyBlue
               ]
    , secondary =
        base
            ++ [ Background.color colors.gray2
               ]
    }



-- C look for buttons.secondary


base : List (Attribute msg)
base =
    [ Border.roundEach { topLeft = 16, topRight = 0, bottomRight = 16, bottomLeft = 0 }
    , padding 10
    , Font.color colors.blue1
    , Font.size 16

    -- , width (px 128)
    -- , height (px 36)
    , paddingEach { top = 10, right = 19, bottom = 10, left = 22 }
    , Font.semiBold
    , mouseOver
        [ Font.color colors.cremeLight
        , Background.color colors.carminePink
        ]
    ]


link : List (Attribute msg)
link =
    [ Font.underline
    , mouseOver [ Font.color colors.carminePink ]
    , Font.semiBold
    , Font.color colors.gray4
    ]


title : List (Attribute msg)
title =
    [ --Font.size 44
      Font.family [ Font.typeface "Inter" ]
    , Font.color palette.white

    -- , paddingXY 32 0
    ]


menu : List (Attribute msg)
menu =
    [ Font.color palette.white
    , Font.letterSpacing 2
    , Font.size 14
    , Font.family [ Font.typeface "Inter" ]
    , mouseOver
        [ Font.color colors.carminePink
        ]
    ]


btn : List (Attribute msg)
btn =
    [ Border.roundEach { topLeft = 16, topRight = 0, bottomRight = 16, bottomLeft = 0 }
    , Border.color palette.primary
    , Border.width 1
    , padding 10

    -- , Font.color palette.white
    -- , Font.color palette.primary
    , Font.color palette.primary
    , Font.semiBold
    , Font.size 16

    -- , Background.color colors.cremeDark
    -- , width (px 128)
    -- , height (px 36)
    , paddingEach { top = 10, right = 19, bottom = 10, left = 22 }
    , Font.regular
    , mouseOver
        [ Font.color colors.cremeLight
        , Background.color colors.carminePink
        , Border.color colors.carminePink
        ]
    ]


pt : Int -> Attribute msg
pt size =
    paddingEach { top = size, left = 0, right = 0, bottom = 0 }


pr : Int -> Attribute msg
pr size =
    paddingEach { top = 0, left = 0, right = size, bottom = 0 }


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


lineHeight : Float -> Attribute msg
lineHeight size =
    css "line-height" (String.fromFloat size)


minimumWidth : Int -> Attribute msg
minimumWidth size =
    width (fill |> minimum size)


css : String -> String -> Attribute msg
css property value =
    htmlAttribute <| Html.Attributes.style property value


debug : { red : Element.Attr decorative msg, blue : Element.Attr a b, black : Element.Attr c d, misc : Element.Attr e f }
debug =
    { red =
        Background.color colors.red1
    , blue =
        Background.color colors.blue1
    , black = Background.color colors.black1
    , misc =
        Background.color colors.skyBlue
    }
