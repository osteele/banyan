module Dropbox.Extras
    exposing
        ( folder
        , file
        , deleted
        , record
        , size
        , isDir
        , decodeString
        , encodeString
        , listFolderToContinueError
        )

{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder>.
-}

import Char
import Date exposing (Date)
import Dropbox exposing (..)
import Extras exposing (..)
import Hex
import Regex


{-| Fields shared by all records of the Metadata union, that represents
ListFolders entries.
-}
type alias CommonMeta =
    { name : String
    , pathLower : Maybe String
    , pathDisplay : Maybe String
    , parentSharedFolderId : Maybe String
    , clientModified : Maybe Date
    , serverModified : Maybe Date
    }



-- CONSTRUCTORS


{-| Create a DeletedMeta record.
-}
deleted : String -> Metadata
deleted path =
    DeletedMeta
        { name = takeFileName path
        , pathLower = Just <| String.toLower path
        , pathDisplay = Just path
        , parentSharedFolderId = Nothing
        }


{-| Create a FileMeta record, with empty id and rev, and 0 modified times.
-}
file : String -> Int -> Metadata
file path size =
    FileMeta
        { name = takeFileName path
        , pathLower = Just <| String.toLower path
        , pathDisplay = Just <| path
        , size = size
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


{-| Create a FolderMeta record, with empty id.
-}
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



-- PROJECTIONS


{-| Return a record with the common attributes of the union records.
-}
record : Metadata -> CommonMeta
record entry =
    case entry of
        FileMeta data ->
            { name = data.name
            , pathDisplay = data.pathDisplay
            , pathLower = data.pathLower
            , parentSharedFolderId = data.parentSharedFolderId
            , clientModified = Just data.clientModified
            , serverModified = Just data.serverModified
            }

        FolderMeta data ->
            { name = data.name
            , pathDisplay = data.pathDisplay
            , pathLower = data.pathLower
            , parentSharedFolderId = data.parentSharedFolderId
            , clientModified = Nothing
            , serverModified = Nothing
            }

        DeletedMeta data ->
            { name = data.name
            , pathDisplay = data.pathDisplay
            , pathLower = data.pathLower
            , parentSharedFolderId = data.parentSharedFolderId
            , clientModified = Nothing
            , serverModified = Nothing
            }


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
        FileMeta data ->
            Just data.size

        _ ->
            Nothing



-- PREDICATES


isDir : Metadata -> Bool
isDir entry =
    case entry of
        FolderMeta _ ->
            True

        _ ->
            False



-- STRING SERIALIZATION


nameOptionalSizeRe : Regex.Regex
nameOptionalSizeRe =
    Regex.regex "^(.+):(\\d*)$"


pathQuoteRe : Regex.Regex
pathQuoteRe =
    Regex.regex "[;:%]"


pathUnquoteRe : Regex.Regex
pathUnquoteRe =
    Regex.regex "%[0-9a-zA-Z]{2}"


quotePath : String -> String
quotePath =
    Regex.replace Regex.All
        pathQuoteRe
        (\{ match } ->
            String.toList match
                |> List.head
                |> Maybe.map (Char.toCode >> Hex.toString >> (++) "%")
                |> Maybe.withDefault match
        )


unquotePath : String -> String
unquotePath =
    Regex.replace Regex.All
        pathUnquoteRe
        (\{ match } ->
            String.dropLeft 1 match
                |> Hex.fromString
                |> Result.map (Char.fromCode >> String.fromChar)
                |> Result.withDefault match
        )


{-| Construct an entry from a string.

The string is a ;-separated list of paths. Files end in :size, where size
is an optional size.

    fromString "/file" -- file with no size
    fromString "/file:10" -- file with size
    fromString "/dir/" -- folder

-}
decodeString : String -> Metadata
decodeString path =
    if String.endsWith "/" path then
        folder <| unquotePath <| String.dropRight 1 path
    else
        let
            ( p, size ) =
                case path |> Regex.find (Regex.AtMost 1) nameOptionalSizeRe |> List.head |> Maybe.map .submatches of
                    Just ((Just p) :: sizeStr :: _) ->
                        ( unquotePath p
                        , sizeStr
                            |> Maybe.andThen (String.toInt >> Result.toMaybe)
                            |> Maybe.withDefault 0
                        )

                    _ ->
                        ( unquotePath path, 0 )
        in
            file p size


{-| Transform deleted, folder, and file paths respecively into
"-path", "path/", and "path"
-}
affixPathMetadata : Metadata -> String -> String
affixPathMetadata entry path =
    let
        p =
            quotePath path
    in
        case entry of
            DeletedMeta _ ->
                "-" ++ p

            FileMeta { size } ->
                String.join ":" <|
                    List.filterMap identity
                        [ Just p
                        , Maybe.map Basics.toString <| maybeToDefault 0 size
                        ]

            FolderMeta _ ->
                p ++ "/"


{-| Encode a file entry as a path, given a current working directory as context.
Returns a tuple of the path (or null if the entry has no displayPath), and the
new context.
-}
encodeStringS : Maybe String -> Metadata -> ( Maybe String, Maybe String )
encodeStringS cwd entry =
    case record entry |> .pathDisplay of
        Nothing ->
            ( Nothing, cwd )

        Just path ->
            let
                p =
                    affixPathMetadata entry path
            in
                case entry of
                    FolderMeta _ ->
                        ( Just p, Just path )

                    _ ->
                        ( Just p, cwd )


{-| Turn a tree into a string. See toString for the format.
-}
encodeString : Metadata -> String
encodeString entry =
    encodeStringS Nothing entry
        |> Tuple.first
        |> Maybe.withDefault (record entry |> .name |> (++) "…/" |> affixPathMetadata entry)



-- ERRORS


listFolderToContinueError : ListFolderError -> ListFolderContinueError
listFolderToContinueError error =
    case error of
        PathListError e ->
            PathListContinueError e

        OtherListError s v ->
            OtherListContinueError s v

        OtherListFailure e ->
            OtherListContinueFailure e
