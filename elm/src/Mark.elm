module Mark exposing (default, loading, oops, view)

import Element
    exposing
        ( Element
        , fill
        , image
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , text
        , width
        )
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Element.Region
import Html
import Html.Attributes
import Markdown.Block as Block exposing (ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Styles exposing (colors)


default : String -> List (Element msg)
default preview =
    case view preview of
        Ok rendered ->
            rendered

        Err _ ->
            [ oops ]


view : String -> Result String (List (Element msg))
view str =
    str
        |> String.replace "<br>" "\n\n"
        |> Markdown.Parser.parse
        |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.andThen (Markdown.Renderer.render elmUiRenderer)


elmUiRenderer : Markdown.Renderer.Renderer (Element msg)
elmUiRenderer =
    { heading = heading
    , paragraph = Element.paragraph Styles.paragraph
    , thematicBreak = Element.none
    , text = text
    , strong = \content -> Element.row [ Font.bold ] content
    , emphasis = \content -> Element.row [ Font.italic ] content
    , strikethrough = \content -> Element.row [ Font.strike ] content
    , codeSpan = code
    , link =
        \{ destination } body ->
            Element.newTabLink
                [ Element.htmlAttribute (Html.Attributes.style "display" "inline-flex") ]
                { url = destination
                , label =
                    Element.paragraph
                        Styles.link
                        body
                }
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            case image.title of
                Just title ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = title }

                Nothing ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , blockQuote =
        \children ->
            Element.column
                [ Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , padding 10
                , Border.color (Element.rgb255 145 145 145)
                , Element.Background.color (Element.rgb255 245 245 245)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15, width fill ]
                (items
                    |> List.map
                        (\(ListItem task children) ->
                            Element.wrappedRow [ Element.spacing 5, width fill ]
                                [ paragraph
                                    (Styles.paragraph ++ [ Element.alignTop, paddingXY 10 0, width fill ])
                                    ((case task of
                                        IncompleteTask ->
                                            Element.Input.defaultCheckbox False

                                        CompletedTask ->
                                            Element.Input.defaultCheckbox True

                                        NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , html = Markdown.Html.oneOf []
    , table = Element.column []
    , tableHeader = Element.column []
    , tableBody = Element.column []
    , tableRow = Element.row []
    , tableHeaderCell =
        \_ children ->
            Element.paragraph [] children
    , tableCell =
        \_ children ->
            Element.paragraph [] children
    }


code : String -> Element msg
code snippet =
    Element.el
        [ Element.Background.color
            (Element.rgba 0 0 0 0.04)
        , Border.rounded 2
        , Element.paddingXY 5 3
        , Styles.codeFont
        ]
        (Element.text snippet)


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.el
        [ Element.Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.padding 20
        , Styles.codeFont
        ]
        (Element.text details.body)


heading : { level : Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ Font.color colors.black1
        , Font.size
            (case level of
                Block.H1 ->
                    36

                Block.H2 ->
                    24

                _ ->
                    20
            )
        , Font.regular
        , Element.Region.heading (Block.headingLevelToInt level)
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Element.htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        , paddingEach
            { bottom = 0
            , left = 0
            , right = 0
            , top = 20
            }
        , Font.letterSpacing 1
        , Styles.headFont
        ]
        children


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


oops : Element msg
oops =
    text "Oops! We can't find that."


loading : Element msg
loading =
    text "Loading..."
