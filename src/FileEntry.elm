module FileEntry
    exposing
        ( FileEntry(..)
        , key
        , path
        , size
        , isDir
        , decodeFileEntry
        , fromString
        , toString
        )

import Json.Decode exposing (..)
import Regex


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


{-| Construct an entry from a string. This is used in testing.

The string is a ;-separated list of paths. Files end in :size, where size
is an optional size.

    fromString "/file" -- file with no size
    fromString "/file:10" -- file with size
    fromString "/dir/" -- folder

-}
fromString : String -> FileEntry
fromString path =
    if String.endsWith "/" path then
        let
            p =
                String.dropRight 1 path
        in
            Folder { key = String.toLower p, path = p }
    else
        let
            ( p, size ) =
                case path |> Regex.find (Regex.AtMost 1) (Regex.regex "^(.+):(\\d*)$") |> List.head |> Maybe.map .submatches of
                    Just ((Just p) :: size :: _) ->
                        ( p
                        , size
                            |> Maybe.andThen (String.toInt >> Result.toMaybe)
                        )

                    _ ->
                        ( path, Nothing )
        in
            File { key = String.toLower p, path = p, size = size }


{-| Turn a tree into a string. See fromString for the format.
-}
toString : FileEntry -> String
toString entry =
    case entry of
        Deletion { path } ->
            "-" ++ path

        File { path, size } ->
            String.join ":" <|
                List.filterMap identity
                    [ Just <| path
                    , Maybe.map Basics.toString <| size
                    ]

        Folder { path } ->
            path ++ "/"
