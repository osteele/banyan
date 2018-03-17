module FilesComponent exposing (..)

{-|


## The synced file state and the syncronization status.
-}

import Dropbox
import FileTree exposing (FileTree)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ListFolder exposing (..)
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
    | ReceiveListFolderResponse (Result String Dropbox.ListFolderResponse)
    | RestoreFromCacheOrListFolder
    | RestoreFromCache



-- UPDATE


update : Dropbox.UserAuth -> Msg -> FilesComponent -> ( FilesComponent, Cmd Msg )
update auth msg model =
    case msg of
        Changed ->
            ( model, Cmd.none )

        ListFolder ->
            let
                task =
                    Dropbox.listFolder auth
                        { path = ""
                        , recursive = True
                        , includeDeleted = False
                        , includeHasExplicitSharedMembers = False
                        , includeMediaInfo = False
                        }
            in
                ( model, Task.attempt ReceiveListFolderResponse <| Task.mapError toString task )

        ReceiveListFolderResponse result ->
            case result of
                Result.Ok { entries, cursor, hasMore } ->
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
                                let
                                    task =
                                        Dropbox.listFolderContinue auth { cursor = cursor }
                                in
                                    Task.attempt ReceiveListFolderResponse <| Task.mapError toString task
                            else
                                saveFilesCache <| encode m
                    in
                        ( m, cmd )

                Result.Err msg ->
                    { model | hasMore = False, errorMessage = Just <| toString msg }
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
    Sub.none



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
