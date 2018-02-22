module FileEntry exposing (..)


type alias FileEntry =
    { tag : String
    , key : String
    , path : String
    , size : Maybe Int
    }


deleteTag =
    "deleted"


dirTag : String
dirTag =
    "folder"


fileTag =
    "file"


isDir : FileEntry -> Bool
isDir entry =
    entry.tag == dirTag
