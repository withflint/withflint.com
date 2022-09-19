module Blog.Update exposing (init, update)

import Blog.Types exposing (Article, ArticleState(..), Meta, Model, Msg(..))
import Http
import Json.Decode as D
import Json.Decode.Extra exposing (andMap)
import Return exposing (Return, return, singleton)


init : Maybe String -> Return Msg Model
init article =
    return
        { topic = ""
        , articles = []
        , article = Loading
        , viewing = article
        , title = blogDefaultPageTitle
        , isPhoneMenuVisible = False
        }
        (Http.get
            { url = "/articles"
            , expect = Http.expectString ReceiveBlogData
            }
        )


update : Msg -> Model -> Return Msg Model
update msg model =
    Return.map updatePageTitle <|
        case msg of
            ReceiveBlogData result ->
                case result of
                    Ok raw ->
                        case D.decodeString (D.list decode) raw of
                            Ok articles ->
                                case model.viewing of
                                    Nothing ->
                                        singleton
                                            { model
                                                | article = List articles
                                                , articles = articles
                                            }

                                    Just article ->
                                        singleton
                                            { model
                                                | article = chooseArticle articles article
                                                , articles = articles
                                            }

                            Err _ ->
                                singleton { model | article = NotFound }

                    Err _ ->
                        singleton { model | article = NotFound }

            PhoneMenuToggle ->
                singleton { model | isPhoneMenuVisible = not model.isPhoneMenuVisible }

            LoadArticleList ->
                singleton { model | article = List model.articles }

            LoadArticle article ->
                case article of
                    "" ->
                        singleton { model | article = List model.articles }

                    _ ->
                        singleton { model | article = chooseArticle model.articles article }


decode : D.Decoder Article
decode =
    D.succeed Article
        |> andMap (D.field "author" D.string)
        |> andMap (D.field "bio" D.string)
        |> andMap (D.field "link" D.string)
        |> andMap (D.field "avatar" D.string)
        |> andMap (D.field "slug" D.string)
        |> andMap (D.field "date" D.string)
        |> andMap (D.field "title" D.string)
        |> andMap (D.field "sub" D.string)
        |> andMap (D.field "body" D.string)
        |> andMap (D.field "meta" decodeMeta)


decodeMeta : D.Decoder Meta
decodeMeta =
    D.succeed Meta
        |> andMap (D.field "type" D.string)
        |> andMap (D.field "title" D.string)
        |> andMap (D.field "url" D.string)
        |> andMap (D.field "image" D.string)
        |> andMap (D.field "description" D.string)
        |> andMap (D.field "author" D.string)
        |> andMap (D.field "publishedTime" D.string)


chooseArticle : List Article -> String -> ArticleState
chooseArticle articles viewing =
    articles
        |> List.foldr
            (\article_ selected ->
                case selected of
                    Loaded _ ->
                        selected

                    _ ->
                        if article_.slug == viewing then
                            Loaded article_

                        else
                            NotFound
            )
            NotFound


updatePageTitle : Model -> Model
updatePageTitle model =
    case model.viewing of
        Nothing ->
            { model | title = blogDefaultPageTitle }

        Just _ ->
            case model.article of
                Loaded article ->
                    { model | title = article.title ++ "  - Flint" }

                _ ->
                    { model | title = blogDefaultPageTitle }


blogDefaultPageTitle : String
blogDefaultPageTitle =
    "Blog - Flint"
