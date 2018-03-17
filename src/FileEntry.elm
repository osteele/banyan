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

import Date
import Dropbox
import Json.Decode exposing (..)
import Regex
import Utils exposing (..)


-- import Utils exposing (takeFileName)


{-| File metadata, as returned from a listFolders request.

See
<https://www.dropbox.com/developers/documentation/http/documentation#FileMetadata>.

Doesn't record id, client_modified, server_modified, rev, sharing_info,
property_groups, has_explicit_shared_members, content_hash, media_info,
symlink_info.

-}
type alias FileMetadata =
    Dropbox.FileMetadata


{-| Folder metadata, as returned from a listFolders request.

See
<https://www.dropbox.com/developers/documentation/http/documentation#FolderMetadata>.

Doesn't record id, sharing_info, property_groups.

-}
type alias FolderMetadata =
    Dropbox.FolderMetadata


{-| File metadata, as returned from a listFolders request.

See <https://www.dropbox.com/developers/documentation/http/documentation#DeletedMetadata>.

Doesn't record name.

-}
type alias DeletedMetadata =
    Dropbox.DeletedMetadata


type FileEntry
    = File FileMetadata
    | Folder FolderMetadata
    | Deleted DeletedMetadata


deleted : String -> FileEntry
deleted path =
    Deleted
        { name = takeFileName path
        , pathLower = Just <| String.toLower path
        , pathDisplay = Just path
        , parentSharedFolderId = Nothing
        }


file : String -> String -> Maybe Int -> FileEntry
file name path size =
    File
        { name = name
        , pathLower = Just <| String.toLower path
        , pathDisplay = Just <| path
        , size = Maybe.withDefault 0 size
        , id = ""
        , clientModified = Date.fromTime 0
        , serverModified = Date.fromTime 0
        , rev = ""
        , contentHash = Nothing
        , hasExplicitSharedMembers = Nothing
        , mediaInfo = Nothing
        , parentSharedFolderId = Nothing
        , propertyGroups = Nothing
        , sharingInfo = Nothing
        }



-- file : String -> Maybe Int -> FileEntry
-- file path size =
--     File
--         { name = takeFileName <| path
--         , pathLower = Just <| String.toLower path
--         , pathDisplay = Just <| path
--         , size = Maybe.withDefault 0 size
--         , id = ""
--         , clientModified = Date.fromTime 0
--         , serverModified = Date.fromTime 0
--         , rev = ""
--         , contentHash = Nothing
--         , hasExplicitSharedMembers = Nothing
--         , mediaInfo = Nothing
--         , parentSharedFolderId = Nothing
--         , propertyGroups = Nothing
--         , sharingInfo = Nothing
--         }


folder : String -> FileEntry
folder path =
    Folder
        { name = takeFileName path
        , pathLower = Just <| String.toLower path
        , pathDisplay = Just path
        , id = ""
        , sharedFolderId = Nothing
        , parentSharedFolderId = Nothing
        , propertyGroups = Nothing
        , sharingInfo = Nothing
        }


key : FileEntry -> String
key entry =
    let
        k { pathLower } =
            case pathLower of
                Just s ->
                    s

                Nothing ->
                    Debug.crash "missing key"
    in
        case entry of
            File data ->
                k data

            Folder data ->
                k data

            Deleted data ->
                k data


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
    let
        displayPath { name, pathDisplay } =
            Maybe.withDefault ("…/" ++ name) pathDisplay
    in
        case entry of
            File data ->
                displayPath data

            Folder data ->
                displayPath data

            Deleted data ->
                displayPath data


size : FileEntry -> Maybe Int
size entry =
    case entry of
        File { size } ->
            Just size

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
            map3 file (field "name" string) (field "path_display" string) (field "size" <| nullable int)

        "folder" ->
            let
                f name pathDisplay pathLower =
                    folder pathDisplay
            in
                map3 f (field "name" string) (field "path_display" string) (field "path_lower" string)

        "delete" ->
            let
                f name pathDisplay pathLower =
                    deleted pathDisplay
            in
                map3 f (field "name" string) (field "path_display" string) (field "path_lower" string)

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
            file (takeFileName p) p size


{-| Turn a tree into a string. See fromString for the format.
-}
toString : FileEntry -> String
toString entry =
    let
        displayPath { name, pathDisplay } =
            Maybe.withDefault ("…/" ++ name) pathDisplay
    in
        case entry of
            Deleted data ->
                "-" ++ displayPath data

            File ({ pathDisplay, size } as data) ->
                String.join ":" <|
                    List.filterMap identity
                        [ Just <| displayPath data
                        , Maybe.map Basics.toString <| maybeToDefault 0 size
                        ]

            Folder data ->
                displayPath data ++ "/"
