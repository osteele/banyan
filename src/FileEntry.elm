module FileEntry exposing (..)

import Json.Decode exposing (..)


type alias FileData =
    { key : String
    , path : String
    , size : Maybe Int
    }


type alias FolderData =
    { key : String
    , path : String
    }


type alias DeletionData =
    { key : String
    , path : String
    }


type FileEntry
    = File FileData
    | Folder FolderData
    | Deletion DeletionData


key : FileEntry -> String
key entry =
    case entry of
        File { key } ->
            key

        Folder { key } ->
            key

        Deletion { key } ->
            key


path : FileEntry -> String
path entry =
    case entry of
        File { path } ->
            path

        Folder { path } ->
            path

        Deletion { path } ->
            path


size : FileEntry -> Maybe Int
size entry =
    case entry of
        File { size } ->
            size

        _ ->
            Nothing


decodeFileEntry : Decoder FileEntry
decodeFileEntry =
    field "tag" string
        |> andThen decoderFor


decoderFor : String -> Decoder FileEntry
decoderFor tag =
    case tag of
        "file" ->
            map3 FileData (field "key" string) (field "path" string) (field "size" <| nullable int)
                |> map File

        "folder" ->
            map2 FolderData (field "key" string) (field "path" string)
                |> map Folder

        "delete" ->
            map2 DeletionData (field "key" string) (field "path" string)
                |> map Deletion

        _ ->
            fail <|
                "Trying to decode Dropbox file entry, but received unknown tag "
                    ++ tag


isDir : FileEntry -> Bool
isDir entry =
    case entry of
        Folder _ ->
            True

        _ ->
            False
