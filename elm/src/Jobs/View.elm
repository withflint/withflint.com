module Jobs.View exposing (view)

import Dict
import Element
    exposing
        ( Attribute
        , Element
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , focused
        , height
        , htmlAttribute
        , image
        , link
        , maximum
        , minimum
        , mouseOver
        , newTabLink
        , none
        , padding
        , paddingXY
        , paragraph
        , px
        , row
        , shrink
        , spacing
        , spacingXY
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File
import Jobs.Types exposing (Config, Field(..), Job, Model, Msg(..), View(..))
import Layout exposing (Layout, footer, menu)
import Mark
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors, headFont, headerGradientBackground)
import Text
import Url.Builder exposing (absolute)


view : Model -> Layout Msg
view model =
    { phone =
        [ column
            (width fill :: headerGradientBackground)
            [ phoneHeader
            , workAtPhone model.config
            ]
        , column
            [ wf
            , height fill
            , paddingXY 20 40
            , spacing 50
            , centerX
            ]
            (jobsView phoneView model :: footer.phone)
        ]
    , desktop =
        [ -- top header including hero title and nav bar
          column
            (width fill :: headerGradientBackground)
            [ desktopHeader
            , workAtDesktop model.config
            ]
        , column
            [ width <| maximum 1500 fill
            , height fill
            , paddingXY 100 40
            , spacing 50
            , centerX
            , Background.color colors.cremeLight
            ]
            -- C changed
            -- (jobsView desktopView model :: footer.desktop)
            (jobsView desktopView model :: [])
        ]
    , tablet =
        [ column
            (width fill :: headerGradientBackground)
            [ desktopHeader
            , workAtDesktop model.config
            ]
        , column
            [ width <| maximum 1500 fill
            , height fill
            , paddingXY 100 40
            , spacing 50
            , centerX
            ]
            (jobsView desktopView model :: footer.tablet)
        ]
    }


phoneHeader : Element Msg
phoneHeader =
    row [ wf, paddingXY 30 0 ]
        [ row [ width <| maximum 1500 fill, paddingXY 0 40, centerX ]
            [ Element.link []
                { url = toPath Home
                , label =
                    Element.image [ centerY, alignLeft, width (px 100), height (px 50) ]
                        { src = "/static/images/logo-white.svg?new"
                        , description = "Flint"
                        }
                }
            ]
        ]



-- menu bar


desktopHeader : Element Msg
desktopHeader =
    row
        [ wf
        , paddingXY 100 0
        ]
        [ row [ width <| maximum 1300 fill, paddingXY 0 40, centerX ]
            [ column [ wf ]
                [ Element.link []
                    { url = toPath Home
                    , label =
                        Element.image [ centerY, alignLeft, width (px 100), height (px 50) ]
                            { src = "/static/images/logo-white.svg?new"
                            , description = "Flint"
                            }
                    }
                ]
            , column [ wf, alignRight ]
                [ column (wf :: Styles.paragraph)
                    [ row [ spacingXY 30 0, alignRight ] <|
                        List.map
                            (\( path, label ) ->
                                row []
                                    [ link
                                        [ padding 5

                                        -- C color changed
                                        , Font.color colors.cremeLight
                                        , headFont
                                        , Font.size 16
                                        , mouseOver [ Font.color colors.carminePink ]
                                        ]
                                        { url = toPath path
                                        , label = text label
                                        }
                                    ]
                            )
                            menu
                    ]
                ]
            ]
        ]



-- Launch your nursing career in America


workAtDesktop : Config -> Element Msg
workAtDesktop config =
    row
        [ wf
        , padding 50
        ]
        [ column [ wf, spacing 30 ]
            [ paragraph
                [ width <| maximum 1400 fill
                , centerX
                , centerY
                , Font.center
                , height (minimum 150 shrink)
                , Font.color colors.white3
                , Font.size 70

                -- title font color
                , Font.color colors.cremeLight
                , Styles.headFont
                ]
                [ text config.copy.title
                ]
            ]
        ]


workAtPhone : Config -> Element Msg
workAtPhone config =
    row [ wf, Background.color colors.blue1, padding 50 ]
        [ column [ wf, spacing 30 ]
            [ paragraph
                [ wf
                , centerX
                , centerY
                , Font.center
                , height (minimum 50 shrink)
                , Font.color colors.white3
                , Font.size 40

                -- title font color
                , Font.color colors.cremeLight
                , Styles.headFont
                ]
                [ text config.copy.title
                ]
            ]
        ]


type alias Viewer =
    { jobView : ( String, String, Job ) -> Element Msg
    , applyView : Job -> Model -> Element Msg
    , copyView : Config -> Element Msg
    }


desktopView : Viewer
desktopView =
    { jobView = desktopJobView
    , applyView = desktopApplyView
    , copyView = desktopCopyView
    }


phoneView : Viewer
phoneView =
    { jobView = phoneJobView
    , applyView = phoneApplyView
    , copyView = phoneCopyView
    }


jobsView : Viewer -> Model -> Element Msg
jobsView viewer model =
    case model.view of
        JobsView ->
            if Dict.isEmpty model.jobs then
                column []
                    [ text "Sorry, no positions are currently open!"
                    ]

            else
                column [ wf, spacingXY 0 20 ] <|
                    [ viewer.copyView model.config
                    , column [ wf, spacing 40, paddingXY 0 40 ]
                        (paragraph
                            [ Font.size 24
                            , Styles.headFont

                            -- C added
                            , Font.color colors.blue1
                            ]
                            [ text "Open Positions"
                            ]
                            :: (Dict.toList model.jobs |> List.map (\( id, job ) -> ( model.config.page, id, job )) |> List.map viewer.jobView)
                        )
                    ]

        ApplyView jobId ->
            case Dict.get jobId model.jobs of
                Just job ->
                    column [ centerX, spacingXY 0 100, wf, centerX ]
                        [ column [ width <| maximum 800 fill, centerX ]
                            [ paragraph Styles.heading
                                [ text job.title
                                ]
                            , column (Styles.paragraph ++ [ spacing 20 ])
                                (case job.description of
                                    "" ->
                                        [ none ]

                                    desc ->
                                        Mark.default desc
                                )
                            ]
                        , viewer.applyView job model
                        ]

                Nothing ->
                    jobsView viewer { model | view = JobsView }


desktopJobView : ( String, String, Job ) -> Element Msg
desktopJobView ( page, id, job ) =
    row [ wf ]
        [ column [ alignLeft, spacingXY 0 10, wf ]
            [ link
                [ Font.color colors.blue1
                , mouseOver
                    [ -- C changed color
                      Font.color colors.carminePink
                    ]
                ]
                { url = absolute [ page, id ] []
                , label = paragraph [ wf, spacing 10 ] [ text job.title ]
                }
            , wrappedRow [ spacingXY 10 10, Font.size 15, wf ]
                [ text job.location
                , text job.equity
                , text job.experience
                ]
            ]
        , column [ height fill, alignTop, alignRight ]
            [ Input.button (centerY :: Font.size 15 :: Styles.buttons.primary)
                { onPress = Just (Apply True id)
                , label = text "Apply Now"
                }
            ]
        ]


phoneJobView : ( String, String, Job ) -> Element Msg
phoneJobView ( page, id, job ) =
    row [ wf ]
        [ column [ alignLeft, spacingXY 0 10, wf ]
            [ paragraph [ wf ]
                [ link
                    [ Font.color colors.blue1
                    , mouseOver
                        [ -- C changed color
                          Font.color colors.carminePink
                        ]
                    ]
                    { url = absolute [ page, id ] []
                    , label = paragraph [ wf, spacing 10 ] [ text job.title ]
                    }
                ]
            , wrappedRow [ spacingXY 10 10, Font.size 15, wf ]
                [ text job.location
                , text job.equity
                , text job.experience
                ]
            ]
        , column [ alignRight ]
            [ Input.button (centerY :: Font.size 15 :: Styles.buttons.primary)
                { onPress = Just (Apply True id)
                , label = text "Apply Now"
                }
            ]
        ]


textbox : List (Attribute msg)
textbox =
    [ Border.width 1
    , padding 7
    , focused [ Border.color colors.blue1 ]
    ]


multitextbox : List (Attribute msg)
multitextbox =
    [ Border.width 1
    , padding 7
    , height (px 100)
    , focused [ Border.color colors.blue1 ]
    ]


textboxLabel : List (Attribute msg)
textboxLabel =
    [ Font.size 15
    , alignTop
    , wf
    ]


smallHeading : List (Attribute msg)
smallHeading =
    [ Font.size 24
    , Styles.font
    ]


desktopApplyView : Job -> Model -> Element Msg
desktopApplyView job model =
    column [ centerX, spacing 10, width <| minimum 300 <| maximum 500 fill ] <|
        [ el smallHeading <| text "Apply"
        , Input.username textbox
            { onChange = Set FirstName
            , text = Text.toString model.applicant.firstName
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "First Name"
            }
        , Input.username textbox
            { onChange = Set LastName
            , text = Text.toString model.applicant.lastName
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "Last Name"
            }
        , Input.email textbox
            { onChange = Set Email
            , text = Text.toString model.applicant.email
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "Email"
            }
        , Input.text textbox
            { onChange = Set Phone
            , text = Text.toString model.applicant.phone
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "Phone"
            }
        , Input.button (Font.size 15 :: Styles.buttons.primary)
            { onPress = Just UploadResume
            , label = text "Upload Resume"
            }
        , case model.applicant.resume of
            Just file ->
                el [ Font.size 15 ] <| text <| File.name file

            Nothing ->
                none
        , Input.multiline multitextbox
            { onChange = Set Reason
            , text = Text.toString model.applicant.reason
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text model.config.copy.why
            , spellcheck = True
            }
        , case model.error of
            Just err ->
                paragraph [ Font.size 15 ]
                    [ text err
                    ]

            Nothing ->
                case model.success of
                    Just msg ->
                        paragraph [ Font.size 15 ]
                            [ text msg
                            ]

                    Nothing ->
                        none
        ]
            ++ (case model.success of
                    Just _ ->
                        []

                    Nothing ->
                        [ Input.button (Font.size 15 :: Styles.buttons.primary)
                            { onPress = Just (Submit job)
                            , label = text "Submit"
                            }
                        ]
               )


phoneApplyView : Job -> Model -> Element Msg
phoneApplyView job model =
    column [ centerX, spacing 10, wf ] <|
        [ el smallHeading <| text "Apply"
        , Input.username textbox
            { onChange = Set FirstName
            , text = Text.toString model.applicant.firstName
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "First Name"
            }
        , Input.username textbox
            { onChange = Set LastName
            , text = Text.toString model.applicant.lastName
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "Last Name"
            }
        , Input.email textbox
            { onChange = Set Email
            , text = Text.toString model.applicant.email
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "Email"
            }
        , Input.text textbox
            { onChange = Set Phone
            , text = Text.toString model.applicant.phone
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| text "Phone"
            }
        , Input.button (Font.size 15 :: Styles.buttons.primary)
            { onPress = Just UploadResume
            , label = text "Upload Resume"
            }
        , case model.applicant.resume of
            Just file ->
                el [ Font.size 15 ] <| text <| File.name file

            Nothing ->
                none
        , Input.multiline multitextbox
            { onChange = Set Reason
            , text = Text.toString model.applicant.reason
            , placeholder = Nothing
            , label = Input.labelAbove textboxLabel <| paragraph [] [ text model.config.copy.why ]
            , spellcheck = True
            }
        , case model.error of
            Just err ->
                paragraph [ Font.size 15 ]
                    [ text err
                    ]

            Nothing ->
                case model.success of
                    Just msg ->
                        paragraph [ Font.size 15 ]
                            [ text msg
                            ]

                    Nothing ->
                        none
        ]
            ++ (case model.success of
                    Just _ ->
                        []

                    Nothing ->
                        [ Input.button (Font.size 15 :: Styles.buttons.primary)
                            { onPress = Just (Submit job)
                            , label = text "Submit"
                            }
                        ]
               )


desktopCopyView : Config -> Element Msg
desktopCopyView config =
    column [ spacing 50, wf, height fill ]
        ([ paragraph [ wf, height fill, Font.size 30, Font.color colors.blue1, Styles.headFont ]
            [ text config.copy.desktopHeader
            ]
         , row [ wf, spacing 50, height fill ]
            [ column [ wf, alignTop ]
                [ paragraph Styles.paragraph
                    [ text config.copy.paragraph1
                    ]
                ]
            , column [ wf, alignTop ]
                [ paragraph Styles.paragraph
                    (text config.copy.paragraph2 :: Maybe.withDefault [] config.copy.other)
                ]
            ]
         ]
            ++ (if config.page == "join" then
                    [ hiringProcess ]

                else
                    []
               )
        )


phoneCopyView : Config -> Element Msg
phoneCopyView config =
    column [ spacing 50, wf, height fill ]
        ([ paragraph [ wf, height fill, Font.size 30, Font.color colors.blue1, Styles.headFont ]
            [ text config.copy.phoneHeader
            ]
         , column [ wf, spacing 50, height fill ]
            [ column [ wf, alignTop ]
                [ paragraph Styles.paragraph
                    [ text config.copy.paragraph1
                    ]
                ]
            , column [ wf, alignTop ]
                [ paragraph Styles.paragraph
                    (text config.copy.paragraph2 :: Maybe.withDefault [] config.copy.other)
                ]
            ]
         ]
            ++ (if config.page == "join" then
                    [ hiringProcess ]

                else
                    []
               )
        )


hiringProcess : Element msg
hiringProcess =
    row
        [ wf
        ]
        [ paragraph []
            [ newTabLink [ wf ]
                { url = "/static/images/hiring-process.svg?new"
                , label =
                    image [ wf, centerX ]
                        { src = "/static/images/hiring-process.svg?new"
                        , description = "Interview Process"
                        }
                }
            ]
        ]


wf : Attribute msg
wf =
    width fill
