module DropboxExtras
    exposing
        ( folder
        , file
        , deleted
        , key
        , path
        , size
        , isDir
        , decodeFileEntry
        , fromString
        , toString
        , extractAccessToken
        , listFolderToContinueError
        )

{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder>.
-}

import Date
import Dropbox exposing (..)
import Json.Decode exposing (..)
import Regex
import Utils exposing (..)


deleted : String -> Metadata
deleted path =
    DeletedMeta
        { name = takeFileName path
        , pathLower = Just <| String.toLower path
        , pathDisplay = Just path
        , parentSharedFolderId = Nothing
        }


file : String -> String -> Maybe Int -> Metadata
file name path size =
    FileMeta
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


folder : String -> Metadata
folder path =
    FolderMeta
        { name = takeFileName path
        , pathLower = Just <| String.toLower path
        , pathDisplay = Just path
        , id = ""
        , sharedFolderId = Nothing
        , parentSharedFolderId = Nothing
        , propertyGroups = Nothing
        , sharingInfo = Nothing
        }


key : Metadata -> String
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
            FileMeta data ->
                k data

            FolderMeta data ->
                k data

            DeletedMeta data ->
                k data


name : Metadata -> String
name entry =
    case entry of
        FileMeta { name } ->
            name

        FolderMeta { name } ->
            name

        DeletedMeta { name } ->
            name


path : Metadata -> String
path entry =
    let
        displayPath { name, pathDisplay } =
            Maybe.withDefault ("…/" ++ name) pathDisplay
    in
        case entry of
            FileMeta data ->
                displayPath data

            FolderMeta data ->
                displayPath data

            DeletedMeta data ->
                displayPath data


size : Metadata -> Maybe Int
size entry =
    case entry of
        FileMeta { size } ->
            Just size

        _ ->
            Nothing


decodeFileEntry : Decoder Metadata
decodeFileEntry =
    field "tag" string
        |> andThen decoderFor


decoderFor : String -> Decoder Metadata
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


isDir : Metadata -> Bool
isDir entry =
    case entry of
        FolderMeta _ ->
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
fromString : String -> Metadata
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
toString : Metadata -> String
toString entry =
    let
        displayPath { name, pathDisplay } =
            Maybe.withDefault ("…/" ++ name) pathDisplay
    in
        case entry of
            DeletedMeta data ->
                "-" ++ displayPath data

            FileMeta ({ pathDisplay, size } as data) ->
                String.join ":" <|
                    List.filterMap identity
                        [ pathDisplay
                        , Maybe.map Basics.toString <| maybeToDefault 0 size
                        ]

            FolderMeta data ->
                displayPath data ++ "/"


bearerRegex : Regex.Regex
bearerRegex =
    Regex.regex "Bearer \"(.+)\""


extractAccessToken : UserAuth -> Maybe String
extractAccessToken auth =
    -- TODO extract from JSON instead?
    auth |> Basics.toString |> firstMatch bearerRegex


listFolderToContinueError : ListFolderError -> ListFolderContinueError
listFolderToContinueError error =
    case error of
        PathListError e ->
            PathListContinueError e

        OtherListError s v ->
            OtherListContinueError s v

        OtherListFailure e ->
            OtherListContinueFailure e
