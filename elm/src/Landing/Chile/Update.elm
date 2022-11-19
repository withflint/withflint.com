module Landing.Chile.Update exposing (init, update)

import Apply exposing (Applicant, Field(..))
import Browser.Navigation exposing (Key)
import File.Select
import Http
import Landing.Chile.Types exposing (Model, Msg(..))
import Ports
import RemoteData exposing (RemoteData(..))
import Return exposing (Return, return, singleton)
import Text exposing (Text(..))
import Url exposing (Url)


emptyApplicant : Apply.Applicant
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
        , title = "Chile - Flint"
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
                                    { url = "/apply-chile"
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
                singleton { model | error = Just "¡Oh, no! Todos los campos son obligatorios..." }

        SendApplicantData result ->
            case result of
                Ok _ ->
                    return
                        { model | success = Just "Gracias por tu aplicación." }
                        Ports.candidateApply

                Err _ ->
                    singleton { model | error = Just "Ocurrió un error. Intente aplicar de nuevo. Si el problema persiste, envíenos un correo electrónico con su solicitud a apply@withflint.com" }
