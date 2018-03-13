module FilesModel exposing (..)

{-|


## The synced file state and the syncronization status.
-}

import Dropbox
import DropboxUtils exposing (extractAccessToken)
import FileTree exposing (FileTree)
import ListFolder exposing (listFolder)
import Message exposing (..)
import ListFolder exposing (..)


-- UPDATE


type alias FilesModel =
    { fileTree : FileTree
    , hasMore : Bool
    , syncedEntryCount : Int
    , requestCount : Int
    , errorMessage : Maybe String
    }


init : FilesModel
init =
    { fileTree = FileTree.empty
    , hasMore = False
    , syncedEntryCount = 0
    , requestCount = 0
    , errorMessage = Nothing
    }



-- UPDATE


update : Dropbox.UserAuth -> Msg -> FilesModel -> ( FilesModel, Cmd msg )
update auth msg model =
    case msg of
        ListFolder ->
            case extractAccessToken auth of
                Just token ->
                    { init | hasMore = True }
                        ! [ curry listFolder token { path = "", recursive = True, includeDeleted = False } ]

                Nothing ->
                    { model | errorMessage = Just "Failed to extract access token" } ! []

        ReceiveListFolderResponse result ->
            case result of
                Result.Ok ( entries, hasMore ) ->
                    let
                        tree =
                            FileTree.addEntries entries model.fileTree

                        cmd =
                            if hasMore then
                                Cmd.none
                            else
                                saveFilesCache <| FileTree.encode tree
                    in
                        ( { model
                            | fileTree = tree
                            , hasMore = hasMore
                            , syncedEntryCount = model.syncedEntryCount + List.length entries
                            , requestCount = model.requestCount + 1
                          }
                        , cmd
                        )

                Result.Err msg ->
                    { model | hasMore = False, errorMessage = Just msg }
                        ! []

        _ ->
            model ! []



-- SUBSCRIPTIONS


subscriptions : FilesModel -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveFileList (ReceiveListFolderResponse << decodeFileList)
        , receiveFileListError (ReceiveListFolderResponse << Result.Err)
        ]
