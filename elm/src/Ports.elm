port module Ports exposing (candidateApply)


port candidateApplyEvent : () -> Cmd msg


candidateApply : Cmd msg
candidateApply =
    candidateApplyEvent ()
