port module Ports exposing (candidateApply, navMenuToggled, toggleNavMenu)


port candidateApplyEvent : () -> Cmd msg


candidateApply : Cmd msg
candidateApply =
    candidateApplyEvent ()


port toggleNavMenu : () -> Cmd msg


port navMenuToggled : (() -> msg) -> Sub msg
