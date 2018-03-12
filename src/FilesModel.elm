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


type alias FilesModel =
    { fileTree : FileTree
    , syncing : Bool
    , syncedEntryCount : Int
    , requestCount : Int
    , errorMessage : Maybe String
    }


init : FilesModel
init =
    { fileTree = FileTree.empty
    , syncing = False
    , syncedEntryCount = 0
    , requestCount = 0
    , errorMessage = Nothing
    }


update : Dropbox.UserAuth -> Msg -> FilesModel -> ( FilesModel, Cmd msg )
update auth msg model =
    case msg of
        SyncFiles ->
            case extractAccessToken auth of
                Just token ->
                    let
                        files =
                            init
                    in
                        { files | syncing = True }
                            ! [ curry listFolder token { path = "", recursive = True, includeDeleted = False } ]

                Nothing ->
                    model ! []

        ReceiveListFolderResponse result ->
            case result of
                Result.Ok ( entries, hasMore ) ->
                    { model
                        | fileTree = FileTree.addEntries entries model.fileTree
                        , syncing = hasMore
                        , syncedEntryCount = model.syncedEntryCount + List.length entries
                        , requestCount = model.requestCount + 1
                    }
                        ! []

                Result.Err msg ->
                    { model
                        | syncing = False
                        , errorMessage = Just msg
                    }
                        ! []

        _ ->
            model ! []


subscriptions : FilesModel -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveFileList (ReceiveListFolderResponse << decodeFileList)
        , receiveFileListError (ReceiveListFolderResponse << Result.Err)
        ]
