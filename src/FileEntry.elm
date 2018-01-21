module FileEntry exposing (..)


type alias FileEntry =
    { tag : String
    , key : String
    , path : String
    , size : Maybe Int
    }


dirTag : String
dirTag =
    "folder"


isDir : FileEntry -> Bool
isDir entry =
    entry.tag == dirTag
