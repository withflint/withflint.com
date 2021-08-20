module Text exposing (Text(..), isValid, toString)


type Text
    = Invalid String
    | Valid String
    | Empty


toString : Text -> String
toString text =
    case text of
        Empty ->
            ""

        Valid string ->
            string

        Invalid string ->
            string


isValid : Text -> Bool
isValid text =
    case text of
        Valid _ ->
            True

        _ ->
            False
