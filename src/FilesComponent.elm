module FilesComponent
    exposing
        ( Model
        , Msg(..)
        , Status(..)
        , fromCache
        , init
        , isEmpty
        , isSyncing
        , subscriptions
        , syncFraction
        , update
        )

{-|


## The synced file state and the syncronization status.
-}

import Dropbox exposing (..)
import DropboxExtras exposing (listFolderToContinueError)
import FileTree exposing (FileTree)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ListFolder exposing (..)
import Process
import Task
import Time exposing (Time)


-- MODEL


type alias Model =
    { files : FileTree
    , status : Status
    , errorMessage : Maybe String
    , cache : Maybe String
    }


type Status
    = Syncing { entries : Int, requests : Int }
    | Synced { entries : Int, requests : Int }
    | Unsynced
    | Started
    | Decoding


init : Model
init =
    { files = FileTree.empty
    , status = Unsynced
    , errorMessage = Nothing
    , cache = Nothing
    }


fromCache : Maybe String -> Model
fromCache c =
    { init | cache = c }


isEmpty : Model -> Bool
isEmpty =
    .files >> FileTree.isEmpty



--- STATUS


nextStatus : Status -> { c | entries : List a, hasMore : Bool } -> Status
nextStatus status { entries, hasMore } =
    let
        data =
            syncStats status

        state =
            if hasMore then
                Syncing
            else
                Synced
    in
        state { entries = List.length entries + data.entries, requests = 1 + data.requests }


syncStats : Status -> { entries : Int, requests : Int }
syncStats status =
    case status of
        Syncing data ->
            data

        _ ->
            { entries = 0, requests = 0 }


syncFraction : Model -> Float
syncFraction model =
    case model.status of
        Syncing { requests } ->
            let
                f =
                    toFloat requests
            in
                f / (f + 1.0)

        Synced _ ->
            1.0

        _ ->
            0


isSyncing : Model -> Bool
isSyncing model =
    case model.status of
        Syncing data ->
            True

        Started ->
            True

        Decoding ->
            True

        _ ->
            False



-- MESSAGES


type Msg
    = Changed
    | ListFolder
    | ReceiveListFolderResponse (Result ListFolderContinueError ListFolderResponse)
    | RestoreFromCacheOrListFolder
    | RestoreFromCache



-- UPDATE


update : Dropbox.UserAuth -> Msg -> Model -> ( Model, Cmd Msg )
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
                { model | files = FileTree.empty, status = Started }
                    ! [ Task.attempt ReceiveListFolderResponse <| Task.mapError listFolderToContinueError task
                      , message Changed
                      ]

        ReceiveListFolderResponse result ->
            case result of
                Result.Ok ({ entries, cursor, hasMore } as data) ->
                    let
                        m =
                            { model
                                | files = FileTree.addEntries entries model.files
                                , status = nextStatus model.status data
                            }

                        cmd =
                            if hasMore then
                                let
                                    task =
                                        Dropbox.listFolderContinue auth { cursor = cursor }
                                in
                                    Task.attempt ReceiveListFolderResponse task
                            else
                                Cmd.batch [ saveFilesCache <| encode m, message Changed ]
                    in
                        ( m, cmd )

                Result.Err err ->
                    { model
                        | status = Synced (syncStats model.status)
                        , errorMessage = Just <| toString err
                    }
                        ! []

        RestoreFromCache ->
            case model.cache |> Maybe.map (Decode.decodeString decode) |> Maybe.andThen Result.toMaybe of
                Just m ->
                    { m | status = Synced { entries = 0, requests = 0 } }
                        ! [ message Changed ]

                Nothing ->
                    update auth ListFolder { model | cache = Nothing }

        RestoreFromCacheOrListFolder ->
            case model.cache of
                Just _ ->
                    -- the delay is necessary in order to display the message
                    { model | status = Decoding } ! [ delay (16 * Time.millisecond) RestoreFromCache ]

                Nothing ->
                    update auth ListFolder { model | cache = Nothing }


delay : Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


message : msg -> Cmd msg
message =
    Task.perform identity << Task.succeed



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- CACHE


encode : Model -> Encode.Value
encode { files } =
    Encode.object
        [ ( "files", FileTree.encode files )
        , ( "version", Encode.int 1 )
        ]


decode : Decoder Model
decode =
    let
        decodeVersion1 =
            Decode.field "files" FileTree.decode
                |> Decode.andThen
                    (\t -> Decode.succeed { init | files = t })
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
