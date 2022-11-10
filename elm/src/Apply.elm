module Apply exposing (Applicant, Candidate, Field(..), Job)

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


type alias Candidate =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
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
