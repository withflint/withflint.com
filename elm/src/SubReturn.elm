module SubReturn exposing
    ( SubReturn
    , returnCmd
    , returnSub
    , singletonSub
    )

import SubCmd exposing (SubCmd)


type alias SubReturn msg eff model =
    ( model, SubCmd msg eff )


{-| Create a `SubReturn` from a given `Cmd`
-}
returnCmd : model -> Cmd msg -> SubReturn msg eff model
returnCmd model cmd =
    ( model, SubCmd.cmd cmd )


{-| Create a `SubReturn` from a given `SubCmd`
-}
returnSub : model -> SubCmd msg eff -> SubReturn msg eff model
returnSub model subCmd =
    ( model, subCmd )


{-| Create a `SubReturn` from a given `Model`
-}
singletonSub : model -> SubReturn msg eff model
singletonSub model =
    ( model, SubCmd.none )
