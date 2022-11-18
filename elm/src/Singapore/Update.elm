module Singapore.Update exposing (init, update)

import Apply exposing (Applicant, Field(..))
import Browser.Navigation exposing (Key)
import File.Select
import Http
import Ports
import RemoteData exposing (RemoteData(..))
import Return exposing (Return, return, singleton)
import Singapore.Types exposing (Model, Msg(..))
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


init : String -> Url -> Key -> Return Msg Model
init gitVersion url key =
    return
        { jobs = NotAsked
        , gitVersion = gitVersion
        , applicant = emptyApplicant
        , error = Nothing
        , title = "Singapore - Flint"
        , url = url
        , key = key
        , success = Nothing
        , isPhoneMenuVisible = False
        }
        Cmd.none


modifyApplicant : Model -> (Applicant -> Applicant) -> Model
modifyApplicant model f =
    { model | applicant = f model.applicant }


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        ReceiveJobsData response ->
            singleton { model | jobs = response }

        PhoneMenuToggle ->
            singleton { model | isPhoneMenuVisible = not model.isPhoneMenuVisible }

        UploadResume ->
            return model <| File.Select.file [ "docx", "pdf", "doc", "*" ] Resume

        Set field contents ->
            singleton <|
                modifyApplicant model
                    (\applicant ->
                        case field of
                            FirstName ->
                                { applicant | firstName = Valid contents }

                            LastName ->
                                { applicant | lastName = Valid contents }

                            Email ->
                                { applicant | email = Valid contents }

                            Phone ->
                                { applicant | phone = Valid contents }

                            Reason ->
                                { applicant | reason = Valid contents }
                    )

        Resume file ->
            singleton <|
                modifyApplicant model
                    (\applicant ->
                        { applicant | resume = Just file }
                    )

        Submit job ->
            let
                valid =
                    List.all Text.isValid
                        [ model.applicant.firstName
                        , model.applicant.lastName
                        , model.applicant.email
                        , model.applicant.phone
                        , model.applicant.reason
                        ]
            in
            if valid then
                return { model | error = Nothing, success = Nothing }
                    (Maybe.withDefault Cmd.none
                        (Maybe.map
                            (\file ->
                                Http.post
                                    { url = "/apply-australia"
                                    , body =
                                        Http.multipartBody
                                            [ Http.stringPart "applicationTitle" job.title
                                            , Http.stringPart "firstName" <| Text.toString model.applicant.firstName
                                            , Http.stringPart "lastName" <| Text.toString model.applicant.lastName
                                            , Http.stringPart "email" <| Text.toString model.applicant.email
                                            , Http.stringPart "phone" <| Text.toString model.applicant.phone
                                            , Http.stringPart "reason" <| Text.toString model.applicant.reason
                                            , Http.filePart "resume" file
                                            ]
                                    , expect = Http.expectWhatever SendApplicantData
                                    }
                            )
                            model.applicant.resume
                        )
                    )

            else
                singleton { model | error = Just "Oh no! All fields are required..." }

        SendApplicantData result ->
            case result of
                Ok _ ->
                    return
                        { model | success = Just "Thank you for your application." }
                        Ports.candidateApply

                Err _ ->
                    singleton { model | error = Just "An error occurred. Please try applying again. If the problem persists, please email us your application at apply@withflint.com" }
