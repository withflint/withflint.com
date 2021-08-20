module Jobs.View exposing (flintCopy, healthCareCopy, view)

import Blog.View exposing (elmUiRenderer)
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
        , link
        , maximum
        , minimum
        , mouseOver
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
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File
import Jobs.Types exposing (Config, Copy, Field(..), Job, Model, Msg(..), View(..))
import Layout exposing (Layout, footer, menu)
import Markdown.Parser
import Markdown.Renderer
import Router.Routes exposing (Page(..), toPath)
import Styles exposing (colors)
import Text


view : Model -> Layout Msg
view model =
    { phone =
        [ phoneHeader
        , workAtPhone model.config
        , column
            [ width fill
            , height fill
            , paddingXY 20 40
            , spacing 50
            , centerX
            ]
            (jobsView phoneView model :: footer.phone)
        ]
    , desktop =
        [ desktopHeader
        , workAtDesktop model.config
        , column
            [ width <| maximum 1500 fill
            , height fill
            , paddingXY 100 40
            , spacing 50
            , centerX
            ]
            (jobsView desktopView model :: footer.desktop)
        ]
    , tablet =
        [ desktopHeader
        , workAtDesktop model.config
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
    row [ width fill, Background.color colors.blue1, paddingXY 30 0 ]
        [ row [ width <| maximum 1500 fill, paddingXY 0 40, centerX ]
            [ Element.link []
                { url = toPath Home
                , label =
                    Element.image [ centerY, alignLeft, width (px 100), height (px 50) ]
                        { src = "/static/images/logo-white.svg"
                        , description = "Flint"
                        }
                }
            ]
        ]


desktopHeader : Element Msg
desktopHeader =
    row [ width fill, Background.color colors.blue1, paddingXY 100 0 ]
        [ row [ width <| maximum 1300 fill, paddingXY 0 40, centerX ]
            [ column [ width fill ]
                [ Element.link []
                    { url = toPath Home
                    , label =
                        Element.image [ centerY, alignLeft, width (px 100), height (px 50) ]
                            { src = "/static/images/logo-white.svg"
                            , description = "Flint"
                            }
                    }
                ]
            , column [ width fill, alignRight ]
                [ column (width fill :: Styles.paragraph)
                    [ row [ spacingXY 30 0, alignRight ] <|
                        List.map
                            (\( path, label ) ->
                                row []
                                    [ link [ padding 5, Font.color colors.white3 ]
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


workAtDesktop : Config -> Element Msg
workAtDesktop config =
    row [ width fill, Background.color colors.blue1, padding 50 ]
        [ column [ width fill, spacing 30 ]
            [ paragraph
                [ width <| maximum 1400 fill
                , centerX
                , centerY
                , Font.center
                , height (minimum 150 shrink)
                , Font.color colors.white3
                , Styles.headFont
                , Font.size 70
                ]
                [ text config.copy.title
                ]
            ]
        ]


workAtPhone : Config -> Element Msg
workAtPhone config =
    row [ width fill, Background.color colors.blue1, padding 50 ]
        [ column [ width fill, spacing 30 ]
            [ paragraph
                [ width fill
                , centerX
                , centerY
                , Font.center
                , height (minimum 50 shrink)
                , Font.color colors.white3
                , Styles.headFont
                , Font.size 40
                ]
                [ text config.copy.title
                ]
            ]
        ]


type alias Viewer =
    { jobView : ( String, Job ) -> Element Msg
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
    if Dict.isEmpty model.jobs then
        column []
            [ text "Sorry, no positions are currently open!"
            ]

    else
        case model.view of
            JobsView ->
                column [ width fill, spacingXY 0 20 ]
                    (viewer.copyView model.config
                        :: (Dict.toList model.jobs |> List.map viewer.jobView)
                    )

            ApplyView jobId ->
                case Dict.get jobId model.jobs of
                    Just job ->
                        column [ centerX, spacingXY 0 100 ]
                            [ column [ width <| maximum 800 fill ]
                                [ paragraph Styles.heading
                                    [ text job.title
                                    ]
                                , column (Styles.paragraph ++ [ spacing 20 ])
                                    (case job.description of
                                        "" ->
                                            [ none ]

                                        desc ->
                                            Markdown.Parser.parse desc
                                                |> Result.mapError (always "error in rendering markdown")
                                                |> Result.andThen (Markdown.Renderer.render elmUiRenderer)
                                                |> Result.withDefault [ none ]
                                    )
                                ]
                            , viewer.applyView job model
                            ]

                    Nothing ->
                        jobsView viewer { model | view = JobsView }


desktopJobView : ( String, Job ) -> Element Msg
desktopJobView ( id, job ) =
    row [ width fill ]
        [ column [ alignLeft, spacingXY 0 10 ]
            [ link [ Font.color colors.blue1, mouseOver [ Font.color colors.blue1 ] ]
                { url = id
                , label = text job.title
                }
            , row [ spacingXY 10 10, Font.size 15, width fill ]
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


phoneJobView : ( String, Job ) -> Element Msg
phoneJobView ( id, job ) =
    row [ width fill ]
        [ column [ alignLeft, spacingXY 0 10 ]
            [ paragraph []
                [ link [ Font.color colors.blue1, mouseOver [ Font.color colors.blue1 ] ]
                    { url = id
                    , label = text job.title
                    }
                ]
            , column [ spacingXY 10 10, Font.size 15, width fill ]
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
    , width fill
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
    column [ centerX, spacing 10 ] <|
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


desktopCopyView : Config -> Element Msg
desktopCopyView config =
    column [ spacing 50, width fill, height fill ]
        [ paragraph [ width fill, height fill, Styles.headFont, Font.size 30 ]
            [ text config.copy.desktopHeader
            ]
        , row [ width fill, spacing 50, height fill ]
            [ column [ width fill, alignTop ]
                [ paragraph Styles.paragraph
                    [ text config.copy.paragraph1
                    ]
                ]
            , column [ width fill, alignTop ]
                [ paragraph Styles.paragraph
                    (text config.copy.paragraph2 :: Maybe.withDefault [] config.copy.other)
                ]
            ]
        ]


phoneCopyView : Config -> Element Msg
phoneCopyView config =
    column [ spacing 50, width fill, height fill ]
        [ paragraph [ width fill, height fill, Styles.headFont, Font.size 30 ]
            [ text config.copy.phoneHeader
            ]
        , column [ width fill, spacing 50, height fill ]
            [ column [ width fill, alignTop ]
                [ paragraph Styles.paragraph
                    [ text config.copy.paragraph1
                    ]
                ]
            , column [ width fill, alignTop ]
                [ paragraph Styles.paragraph
                    (text config.copy.paragraph2 :: Maybe.withDefault [] config.copy.other)
                ]
            ]
        ]


flintCopy : Copy
flintCopy =
    { desktopHeader = "We work with the very best."
    , phoneHeader = "We work with the very best."
    , paragraph1 = "At Flint, we're committed to hiring the best people to build our teams. Building great products takes smart, disciplined, and empathetic individuals who can understand what job the products need to get done and imagine innovative ways to achieve it. Thus we designed the hiring process to help us identify those people."
    , paragraph2 = "We foster a culture of respect, dialogue and growth where our team members can engage in a continuous conversation about product, engineering, and learning."
    , why = "Why do you want to work at Flint?"
    , title = "Work at Flint"
    , pageTitle = "Jobs - Flint"
    , other =
        Just
            [ text " "
            , link Styles.link
                { url = toPath (Blog "culture")
                , label = text "Read more about our values and culture."
                }
            ]
    }


healthCareCopy : Copy
healthCareCopy =
    { desktopHeader = "We work with the very best. Quality candidates lead to quality health outcomes."
    , phoneHeader = "We work with the very best."
    , paragraph1 = "At Flint, we're committed to finding the best people to staff health care teams. We work with highly internationally educated health care professionals who display care for their patients, have quality communication skills, good empathy skills, are attentive to details, can solve problems, and display autonomy and compliances with the standards can think critically and improve the American healthcare system."
    , paragraph2 = "We work with internationally educated health care workers from around the world for staffing opportunities in the United States of America. We offer an all-inclusive solution for the workers to have a seamless transition into their new life in America. We pay for certain legal and processing fees, licensing and offer premium placement."
    , why = "Why do you want to work in the United States of America?"
    , title = "Work with Flint to launch your career in America"
    , pageTitle = "Health Care Jobs  - Flint"
    , other = Nothing
    }
