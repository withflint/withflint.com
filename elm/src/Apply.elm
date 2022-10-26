module Apply exposing (Applicant, Field(..), Job)

import File exposing (File)
import Text exposing (Text)


type alias Applicant =
    { firstName : Text
    , lastName : Text
    , email : Text
    , phone : Text
    , resume : Maybe File
    , reason : Text
    }


type Field
    = FirstName
    | LastName
    | Email
    | Phone
    | Reason


type alias Job =
    { url : String
    , title : String
    , location : String
    , equity : String
    , experience : String
    , description : String
    }
