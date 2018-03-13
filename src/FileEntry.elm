module FileEntry
    exposing
        ( FileEntry(..)
        , folder
        , file
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


{-| File metadata, as returned from a listFolders request.

See
<https://www.dropbox.com/developers/documentation/http/documentation#FileMetadata>.

Doesn't record name, id, client_modified, server_modified, rev, sharing_info,
property_groups, has_explicit_shared_members, content_hash, media_info,
symlink_info.

-}
type alias FileMetadata =
    { key : String
    , path : String
    , size : Maybe Int
    }


{-| Folder metadata, as returned from a listFolders request.

See
<https://www.dropbox.com/developers/documentation/http/documentation#FolderMetadata>.

Renames path_lower -> key, path_display -> path.

Doesn't record name, id, sharing_info, property_groups.

-}
type alias FolderMetadata =
    { key : String
    , path : String
    }


{-| File metadata, as returned from a listFolders request.

See <https://www.dropbox.com/developers/documentation/http/documentation#DeletedMetadata>.

Doesn't record name.

-}
type alias DeletedMetadata =
    { key : String
    , path : String
    }


type FileEntry
    = File FileMetadata
    | Folder FolderMetadata
    | Deletion DeletedMetadata


file : String -> Maybe Int -> FileEntry
file path size =
    File { key = String.toLower path, path = path, size = size }


folder : String -> FileEntry
folder path =
    Folder { key = String.toLower path, path = path }


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
            map3 FileMetadata (field "key" string) (field "path" string) (field "size" <| nullable int)
                |> map File

        "folder" ->
            map2 FolderMetadata (field "key" string) (field "path" string)
                |> map Folder

        "delete" ->
            map2 DeletedMetadata (field "key" string) (field "path" string)
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
