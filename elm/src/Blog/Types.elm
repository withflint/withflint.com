module Blog.Types exposing (Article, ArticleState(..), Meta, Model, Msg(..))

import Http


type alias Model =
    { topic : String
    , article : ArticleState
    , articles : List Article
    , viewing : Maybe String
    , title : String
    }


type ArticleState
    = Loading
    | List (List Article)
    | Loaded Article
    | NotFound


type alias Article =
    { author : String
    , bio : String
    , link : String
    , avatar : String
    , slug : String
    , date : String
    , title : String
    , sub : String
    , body : String
    , meta : Meta
    }


type alias Meta =
    { type_ : String
    , title : String
    , url : String
    , image : String
    , description : String
    , author : String
    , publishedTime : String
    }


type Msg
    = ReceiveBlogData (Result Http.Error String)
    | LoadArticle String
    | LoadArticleList
