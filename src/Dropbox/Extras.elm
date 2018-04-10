module Dropbox.Extras
    exposing
        ( folder
        , file
        , deleted
        , info
        , size
        , isDir
        , listFolderToContinueError
        )

{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder>.
-}

import Date exposing (Date)
import Dropbox exposing (..)
import Extras exposing (..)


{-| Fields shared by all records of the Metadata union, that represents
ListFolders entries.
-}
type alias CommonMeta =
    { name : String
    , pathLower : Maybe String
    , pathDisplay : Maybe String
    , size : Maybe Int
    , clientModified : Maybe Date
    , serverModified : Maybe Date
    }



-- CONSTRUCTORS


{-| Create a DeletedMeta record.
-}
deleted : String -> Metadata
deleted path =
    let
        { name, pathLower, pathDisplay } =
            pathAttrs path
    in
        DeletedMeta
            { name = name
            , pathLower = pathLower
            , pathDisplay = pathDisplay
            }


{-| Create a FileMeta record, with empty id and rev, and 0 modified times.
-}
file : String -> Int -> Metadata
file path size =
    let
        { name, pathLower, pathDisplay } =
            pathAttrs path
    in
        FileMeta
            { name = name
            , pathLower = pathLower
            , pathDisplay = pathDisplay
            , size = size
            , id = ""
            , clientModified = Date.fromTime 0
            , serverModified = Date.fromTime 0
            , rev = ""
            , contentHash = Nothing
            , hasExplicitSharedMembers = Nothing
            , mediaInfo = Nothing
            , propertyGroups = Nothing
            , sharingInfo = Nothing
            }


{-| Create a FolderMeta record, with empty id.
-}
folder : String -> Metadata
folder path =
    let
        { name, pathLower, pathDisplay } =
            pathAttrs path
    in
        FolderMeta
            { name = name
            , pathLower = pathLower
            , pathDisplay = pathDisplay
            , id = ""
            , propertyGroups = Nothing
            , sharingInfo = Nothing
            }


pathAttrs : String -> { name : String, pathLower : Maybe String, pathDisplay : Maybe String }
pathAttrs path =
    let
        pathDisplay =
            if String.startsWith "/" path || path == "" then
                Just path
            else
                Nothing
    in
        { name = takeFileName path
        , pathLower = Maybe.map String.toLower pathDisplay
        , pathDisplay = pathDisplay
        }



-- PROPERTIES


{-| Return a record with the common attributes of the Metadata union records.
-}
info : Metadata -> CommonMeta
info entry =
    case entry of
        FileMeta data ->
            { name = data.name
            , pathDisplay = data.pathDisplay
            , pathLower = data.pathLower
            , size = Just data.size
            , clientModified = Just data.clientModified
            , serverModified = Just data.serverModified
            }

        FolderMeta data ->
            { name = data.name
            , pathDisplay = data.pathDisplay
            , pathLower = data.pathLower
            , size = Nothing
            , clientModified = Nothing
            , serverModified = Nothing
            }

        DeletedMeta data ->
            { name = data.name
            , pathDisplay = data.pathDisplay
            , pathLower = data.pathLower
            , size = Nothing
            , clientModified = Nothing
            , serverModified = Nothing
            }


size : Metadata -> Maybe Int
size =
    info >> .size



-- PREDICATES


isDir : Metadata -> Bool
isDir entry =
    case entry of
        FolderMeta _ ->
            True

        _ ->
            False



-- SERIALIZATION
-- ERRORS


{-| Convert a ListFolderError to a ListFolderContinueError, so that the same
`case` statement can process responses from ListFolder and ListFolderContinue.
-}
listFolderToContinueError : ListFolderError -> ListFolderContinueError
listFolderToContinueError error =
    case error of
        PathListError e ->
            PathListContinueError e

        OtherListError s v ->
            OtherListContinueError s v

        OtherListFailure e ->
            OtherListContinueFailure e
