module FileEntry
    exposing
        ( FileEntry(..)
        , FileMetadata
        , FolderMetadata
        , folder
        , file
        , deleted
        , key
        , path
        , size
        , isDir
        , decodeFileEntry
        , fromString
        , toString
        )

{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder>.
-}

import Json.Decode exposing (..)
import Regex
import Utils exposing (takeFileName)


{-| File metadata, as returned from a listFolders request.

See
<https://www.dropbox.com/developers/documentation/http/documentation#FileMetadata>.

Doesn't record id, client_modified, server_modified, rev, sharing_info,
property_groups, has_explicit_shared_members, content_hash, media_info,
symlink_info.

-}
type alias FileMetadata =
    { key : String
    , name : String
    , path : String
    , size : Maybe Int
    }


{-| Folder metadata, as returned from a listFolders request.

See
<https://www.dropbox.com/developers/documentation/http/documentation#FolderMetadata>.

Renames path_lower -> key, path_display -> path.

Doesn't record id, sharing_info, property_groups.

-}
type alias FolderMetadata =
    { key : String
    , name : String
    , path : String
    }


{-| File metadata, as returned from a listFolders request.

See <https://www.dropbox.com/developers/documentation/http/documentation#DeletedMetadata>.

Doesn't record name.

-}
type alias DeletedMetadata =
    { key : String
    , name : String
    , path : String
    }


type FileEntry
    = File FileMetadata
    | Folder FolderMetadata
    | Deleted DeletedMetadata


deleted : String -> FileEntry
deleted path =
    Deleted { key = String.toLower path, name = takeFileName path, path = path }


file : String -> Maybe Int -> FileEntry
file path size =
    File { key = String.toLower path, name = takeFileName path, path = path, size = size }


folder : String -> FileEntry
folder path =
    Folder { key = String.toLower path, name = takeFileName path, path = path }


key : FileEntry -> String
key entry =
    case entry of
        File { key } ->
            key

        Folder { key } ->
            key

        Deleted { key } ->
            key


name : FileEntry -> String
name entry =
    case entry of
        File { name } ->
            name

        Folder { name } ->
            name

        Deleted { name } ->
            name


path : FileEntry -> String
path entry =
    case entry of
        File { path } ->
            path

        Folder { path } ->
            path

        Deleted { path } ->
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
            map4 FileMetadata (field "path_lower" string) (field "name" string) (field "path_display" string) (field "size" <| nullable int)
                |> map File

        "folder" ->
            map3 FolderMetadata (field "path_lower" string) (field "name" string) (field "path_display" string)
                |> map Folder

        "delete" ->
            map3 DeletedMetadata (field "path_lower" string) (field "name" string) (field "path_display" string)
                |> map Deleted

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
        folder <| String.dropRight 1 path
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
            file p size


{-| Turn a tree into a string. See fromString for the format.
-}
toString : FileEntry -> String
toString entry =
    case entry of
        Deleted { path } ->
            "-" ++ path

        File { path, size } ->
            String.join ":" <|
                List.filterMap identity
                    [ Just <| path
                    , Maybe.map Basics.toString <| size
                    ]

        Folder { path } ->
            path ++ "/"
