module Dropbox.Encoding
    exposing
        ( SerializationState
        , decodeString
        , encodeString
        , decodeRelString
        , encodeRelString
        )

import Char
import Dropbox exposing (..)
import Dropbox.Extras exposing (..)
import Extras exposing (..)
import Hex
import Regex


type alias SerializationState =
    Maybe String


relativeFilePrefix : String
relativeFilePrefix =
    "…/"


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


decodeRelString : SerializationState -> String -> Metadata
decodeRelString cwd path =
    let
        absPath =
            if String.startsWith relativeFilePrefix path then
                String.dropLeft (String.length relativeFilePrefix) path
            else if String.startsWith "/" path then
                path
            else
                Maybe.withDefault "/" cwd ++ path

        decodeSize =
            Maybe.andThen (String.toInt >> Result.toMaybe)
                >> Maybe.withDefault 0

        ( filePath, fileSize ) =
            case absPath |> Regex.find (Regex.AtMost 1) nameOptionalSizeRe |> List.head |> Maybe.map .submatches of
                Just ((Just p) :: sizeStr :: _) ->
                    ( p, decodeSize sizeStr )

                _ ->
                    ( absPath, 0 )
    in
        if String.endsWith "/" absPath then
            String.dropRight 1 absPath |> unquotePath |> folder
        else
            file (unquotePath filePath) fileSize


{-| Construct an entry from a string.

The string is a ;-separated list of paths. Files end in :size, where size
is an optional size.

    fromString "/file" -- file with no size
    fromString "/file:10" -- file with size
    fromString "/dir/" -- folder

-}
decodeString : String -> Metadata
decodeString =
    decodeRelString Nothing


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
encodeRelString : SerializationState -> Metadata -> ( Maybe String, SerializationState )
encodeRelString cwd entry =
    case record entry |> .pathDisplay of
        Nothing ->
            ( Nothing, cwd )

        Just absPath ->
            let
                relative prefix to =
                    if String.startsWith prefix (String.toLower to) then
                        String.dropLeft (String.length prefix) to
                    else
                        to

                relPath =
                    cwd
                        |> Maybe.map (flip relative absPath)
                        |> Maybe.withDefault absPath

                newCwd =
                    if isDir entry then
                        Just <| String.toLower <| absPath ++ "/"
                    else
                        cwd
            in
                ( Just <| affixPathMetadata entry relPath
                , newCwd
                )


{-| Serialize a file entry as a string. See decodeString for the format.
-}
encodeString : Metadata -> String
encodeString entry =
    encodeRelString Nothing entry
        |> Tuple.first
        |> Maybe.withDefault
            (record entry
                |> .name
                |> (++) relativeFilePrefix
                |> affixPathMetadata entry
            )
