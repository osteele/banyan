module Data exposing (..)


type alias UserInfo =
    { abbreviated_name : String
    , display_name : String
    , familiar_name : String
    , given_name : String
    , surname : String
    }


type alias AccountInfo =
    { name : UserInfo
    , teamName : String
    }
