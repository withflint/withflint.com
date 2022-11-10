module Jobs.Update exposing (init, update)

import Apply exposing (Applicant, Candidate, Field(..), Job)
import Browser.Navigation exposing (Key, pushUrl)
import Dict exposing (Dict)
import File.Select
import Http
import Jobs.Types exposing (Config, CurrentPage(..), Model, Msg(..), View(..))
import Json.Decode as Decode exposing (Decoder)
import Ports
import RemoteData exposing (RemoteData(..))
import Return exposing (Return, return, singleton)
import Text exposing (Text(..), toString)
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), Parser, parse, s, string)


idParser : Config msg -> Parser (String -> b) b
idParser config =
    s config.page </> string


emptyApplicant : Applicant
emptyApplicant =
    { firstName = Empty
    , lastName = Empty
    , email = Empty
    , phone = Empty
    , resume = Nothing
    , reason = Empty
    }


init : String -> Url -> Key -> Config Msg -> Return Msg Model
init gitVersion url key config =
    return
        { jobs = NotAsked
        , gitVersion = gitVersion
        , applicant = emptyApplicant
        , error = Nothing
        , title = config.copy.pageTitle
        , url = url
        , key = key
        , config = config
        , success = Nothing
        , isPhoneMenuVisible = False
        , view =
            case Maybe.withDefault "" <| parse (idParser config) url of
                "" ->
                    JobsView

                jobId ->
                    ApplyView jobId
        }
        (Http.get
            { url = config.endpoint
            , expect = Http.expectJson (RemoteData.fromResult >> ReceiveJobsData) jobsDecoder
            }
        )


modifyApplicant : Model -> (Applicant -> Applicant) -> Model
modifyApplicant model f =
    { model | applicant = f model.applicant }


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        ReceiveJobsData response ->
            singleton { model | jobs = response }

        Apply urlChange jobId ->
            absolute [ model.config.page, jobId ] []
                |> (if urlChange then
                        pushUrl model.key

                    else
                        always Cmd.none
                   )
                |> return
                    { model
                        | view =
                            ApplyView jobId
                    }

        PhoneMenuToggle ->
            singleton { model | isPhoneMenuVisible = not model.isPhoneMenuVisible }

        SwitchView view ->
            singleton { model | view = view }

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
                                    { url = model.config.apply
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
            let
                candidate : Maybe Candidate
                candidate =
                    if valid then
                        -- assumes String is not empty here
                        { firstName = toString model.applicant.firstName
                        , lastName = toString model.applicant.lastName
                        , email = toString model.applicant.email
                        , phone = toString model.applicant.phone
                        }
                            |> Just

                    else
                        Nothing

                valid =
                    List.all Text.isValid
                        [ model.applicant.firstName
                        , model.applicant.lastName
                        , model.applicant.email
                        , model.applicant.phone
                        , model.applicant.reason
                        ]
            in
            case result of
                Ok _ ->
                    case model.config.page_ of
                        NurseCareersPage ->
                            return { model | success = Just "Thank you for your application." }
                                (Maybe.map Ports.candidateApply candidate
                                    |> Maybe.withDefault Cmd.none
                                )

                        JoinTheTeamPage ->
                            singleton { model | success = Just "Thank you for your application." }

                Err _ ->
                    singleton { model | error = Just "An error occurred. Please try applying again. If the problem persists, please email us your application at join@withflint.com" }


jobsDecoder : Decoder (Dict String Job)
jobsDecoder =
    Decode.dict jobDecoder


jobDecoder : Decoder Job
jobDecoder =
    Decode.map6 Job
        (Decode.field "url" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "location" Decode.string)
        (Decode.field "equity" Decode.string)
        (Decode.field "experience" Decode.string)
        (Decode.field "description" Decode.string)
