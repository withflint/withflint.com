module Join.Update exposing (init, update)

import Apply exposing (Applicant)
import Browser.Navigation exposing (Key)
import Join.Types exposing (Config, Model, Msg(..), View(..))
import RemoteData exposing (RemoteData(..))
import Return exposing (Return, return, singleton)
import Text exposing (Text(..))
import Url exposing (Url)


emptyApplicant : Applicant
emptyApplicant =
    { firstName = Empty
    , lastName = Empty
    , email = Empty
    , phone = Empty
    , resume = Nothing
    , reason = Empty
    }


init : String -> Url -> Key -> Config -> Return Msg Model
init gitVersion url key config =
    return
        { jobs = NotAsked
        , gitVersion = gitVersion
        , applicant = emptyApplicant
        , error = Nothing
        , url = url
        , key = key
        , config = config
        , title = ""
        , success = Nothing
        , isPhoneMenuVisible = False
        , view =
            JobsView
        }
        Cmd.none


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        PhoneMenuToggle ->
            singleton { model | isPhoneMenuVisible = not model.isPhoneMenuVisible }

        SwitchView view ->
            singleton { model | view = view }
