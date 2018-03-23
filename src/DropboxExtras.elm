module DropboxExtras
    exposing
        ( folder
        , file
        , deleted
        , key
        , path
        , size
        , isDir
        , decodeString
        , encodeString
        , listFolderToContinueError
        )

{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder>.
-}

import Char
import Date
import Dropbox exposing (..)
import Extras exposing (..)
import Hex
import Regex


-- CONSTRUCTORS


deleted : String -> Metadata
deleted path =
    DeletedMeta
        { name = takeFileName path
        , pathLower = Just <| String.toLower path
        , pathDisplay = Just path
        , parentSharedFolderId = Nothing
        }


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



-- Regex.regex "%."


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


{-| Construct an entry from a string. This is used in testing.

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


{-| Turn a tree into a string. See toString for the format.
-}
encodeString : Metadata -> String
encodeString entry =
    let
        displayPath { name, pathDisplay } =
            quotePath <| Maybe.withDefault ("…/" ++ name) pathDisplay
    in
        case entry of
            DeletedMeta data ->
                "-" ++ displayPath data

            FileMeta { pathDisplay, size } ->
                String.join ":" <|
                    List.filterMap identity
                        [ pathDisplay
                        , Maybe.map Basics.toString <| maybeToDefault 0 size
                        ]

            FolderMeta data ->
                displayPath data ++ "/"



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
