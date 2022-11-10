port module Ports exposing (..)

import Apply exposing (Candidate)
import Json.Encode as E


port candidateApplyEvent : E.Value -> Cmd msg


candidateApply : Candidate -> Cmd msg
candidateApply candidate =
    candidateApplyEvent <| encodeCandidate candidate


encodeCandidate : Candidate -> E.Value
encodeCandidate { firstName, lastName, email, phone } =
    E.object
        [ ( "firstName", E.string firstName )
        , ( "lastName", E.string lastName )
        , ( "email", E.string email )
        , ( "phone", E.string phone )
        ]
