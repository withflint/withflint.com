port module Ports exposing (..)

import Apply exposing (Candidate)
import Json.Encode as E


port candidateApplyEvent : () -> Cmd msg


candidateApply : Cmd msg
candidateApply =
    candidateApplyEvent ()


encodeCandidate : Candidate -> E.Value
encodeCandidate { firstName, lastName, email, phone } =
    E.object
        [ ( "firstName", E.string firstName )
        , ( "lastName", E.string lastName )
        , ( "email", E.string email )
        , ( "phone", E.string phone )
        ]
