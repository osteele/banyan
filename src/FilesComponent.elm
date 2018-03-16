module FilesComponent exposing (..)

{-|


## The synced file state and the syncronization status.
-}

import Dropbox
import DropboxUtils exposing (extractAccessToken)
import FileEntry exposing (FileEntry)
import FileTree exposing (FileTree)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ListFolder exposing (..)
import ListFolder exposing (listFolder)
import Process
import Task
import Time exposing (Time)


-- MODEL


type alias FilesComponent =
    { fileTree : FileTree
    , hasMore : Bool
    , syncedEntryCount : Int
    , requestCount : Int
    , errorMessage : Maybe String
    , cache : Maybe String
    }


init : FilesComponent
init =
    { fileTree = FileTree.empty
    , hasMore = False
    , syncedEntryCount = 0
    , requestCount = 0
    , errorMessage = Nothing
    , cache = Nothing
    }


fromCache : Maybe String -> FilesComponent
fromCache c =
    { init | cache = c }


isEmpty : FilesComponent -> Bool
isEmpty =
    .fileTree >> FileTree.isEmpty



-- MESSAGES


type Msg
    = Changed
    | ListFolder
    | ReceiveListFolderResponse (Result String ( List FileEntry, Bool ))
    | RestoreFromCacheOrListFolder
    | RestoreFromCache



-- UPDATE


update : Dropbox.UserAuth -> Msg -> FilesComponent -> ( FilesComponent, Cmd Msg )
update auth msg model =
    case msg of
        Changed ->
            ( model, Cmd.none )

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
                        m =
                            { model
                                | fileTree = FileTree.addEntries entries model.fileTree
                                , hasMore = hasMore
                                , syncedEntryCount = model.syncedEntryCount + List.length entries
                                , requestCount = model.requestCount + 1
                            }

                        cmd =
                            if hasMore then
                                Cmd.none
                            else
                                saveFilesCache <| encode m
                    in
                        ( m, cmd )

                Result.Err msg ->
                    { model | hasMore = False, errorMessage = Just msg }
                        ! []

        RestoreFromCache ->
            case model.cache |> Maybe.map (Decode.decodeString decode) |> Maybe.andThen Result.toMaybe of
                Just m ->
                    m ! []

                Nothing ->
                    update auth ListFolder { model | cache = Nothing }

        RestoreFromCacheOrListFolder ->
            case model.cache of
                Just _ ->
                    -- the delay is necessary in order to display the message
                    model ! [ delay (16 * Time.millisecond) RestoreFromCache ]

                Nothing ->
                    update auth ListFolder { model | cache = Nothing }


delay : Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity



-- SUBSCRIPTIONS


subscriptions : FilesComponent -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveFileList (ReceiveListFolderResponse << decodeFileList)
        , receiveFileListError (ReceiveListFolderResponse << Result.Err)
        ]



-- CACHE


encode : FilesComponent -> Encode.Value
encode { fileTree } =
    Encode.object
        [ ( "files", FileTree.encode fileTree )
        , ( "version", Encode.int 1 )
        ]


decode : Decoder FilesComponent
decode =
    let
        decodeVersion1 =
            Decode.field "files" FileTree.decode
                |> Decode.andThen
                    (\t -> Decode.succeed { init | fileTree = t })
    in
        Decode.field "version" Decode.int
            |> Decode.andThen
                (\version ->
                    case version of
                        1 ->
                            decodeVersion1

                        _ ->
                            Decode.fail <| "Unknown version " ++ toString version
                )



-- fromCache : Maybe String -> FilesComponent
-- fromCache =
--     Maybe.map (Decode.decodeString decode)
--         >> Maybe.andThen Result.toMaybe
--         >> Maybe.withDefault init
