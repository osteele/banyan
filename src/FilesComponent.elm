module FilesComponent
    exposing
        ( Model
        , Msg(..)
        , State(..)
        , fromCache
        , init
        , isEmpty
        , isSyncing
        , subscriptions
        , syncFraction
        , update
        , encode
        , decoder
        , encodeState
        , stateDecoder
        )

{-|


## The synced file state and the synchronization status.
-}

import CmdExtras exposing (..)
import Date
import Date.Extra as Date
import Dropbox exposing (..)
import Dropbox.AccountInfo exposing (AccountInfo)
import Dropbox.Extras exposing (listFolderToContinueError)
import Dropbox.FileTree as FileTree exposing (FileTree)
import Extras exposing (maybeToDefault)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import Ports exposing (saveFilesCache)
import Task
import Time exposing (Time)


-- MODEL


type alias Model =
    { files : FileTree
    , state : State
    , error : Maybe String
    , accountId : Maybe String
    , teamId : Maybe String
    , cache : Maybe String
    }


type State
    = Decoding Float
    | FromCache Time
    | StartedSync
    | Syncing { entries : Int, requests : Int }
    | Synced { entries : Int, requests : Int }
    | Unsynced


init : Model
init =
    { files = FileTree.empty
    , state = Unsynced
    , error = Nothing
    , accountId = Nothing
    , teamId = Nothing
    , cache = Nothing
    }


fromCache : Maybe String -> Model
fromCache c =
    { init | cache = c }


isEmpty : Model -> Bool
isEmpty =
    .files >> FileTree.isEmpty



--- STATE


nextSyncState : State -> { a | entries : List Metadata, hasMore : Bool } -> State
nextSyncState state { entries, hasMore } =
    let
        data1 =
            getSyncData state

        data2 =
            { entries = List.length entries + data1.entries
            , requests = 1 + data1.requests
            }
    in
        if hasMore then
            Syncing data2
        else
            Synced data2


getSyncData : State -> { entries : Int, requests : Int }
getSyncData state =
    case state of
        Syncing data ->
            data

        _ ->
            { entries = 0, requests = 0 }


syncFraction : Model -> Float
syncFraction model =
    case model.state of
        Decoding frac ->
            frac

        FromCache _ ->
            1.0

        Syncing { requests } ->
            toFloat requests / toFloat (requests + 1)

        Synced _ ->
            1.0

        _ ->
            0


isSyncing : Model -> Bool
isSyncing model =
    case model.state of
        Decoding _ ->
            True

        StartedSync ->
            True

        Syncing _ ->
            True

        _ ->
            False



-- MESSAGES


type Msg
    = Changed
    | Cleared
    | StartSyncFiles
    | ReceiveListFolderResponse (Result ListFolderContinueError ListFolderResponse)
    | RestoreFromCacheOrListFolder AccountInfo
    | LoadFromCache CacheDecoderState
    | SaveToCache Time



-- UPDATE


update : Dropbox.UserAuth -> Msg -> Model -> ( Model, Cmd Msg )
update auth msg model =
    case msg of
        Changed ->
            ( model, Cmd.none )

        Cleared ->
            ( model, Cmd.none )

        StartSyncFiles ->
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
                { model | files = FileTree.empty, state = StartedSync }
                    ! [ message Cleared
                      , Task.attempt ReceiveListFolderResponse <| Task.mapError listFolderToContinueError task
                      ]

        ReceiveListFolderResponse result ->
            case result of
                Result.Ok ({ entries, cursor, hasMore } as data) ->
                    let
                        m =
                            { model
                                | files = FileTree.addEntries entries model.files
                                , state = nextSyncState model.state data
                            }

                        cmd =
                            if hasMore then
                                Dropbox.listFolderContinue auth { cursor = cursor }
                                    |> Task.attempt ReceiveListFolderResponse
                            else
                                Task.perform SaveToCache Time.now
                    in
                        m ! [ cmd, message Changed ]

                Result.Err err ->
                    { model
                        | state = nextSyncState model.state { entries = [], hasMore = False }
                        , error = Just <| toString err
                    }
                        ! []

        LoadFromCache decoderState ->
            case decodePathBatch decoderState model of
                Result.Ok ( m, Just nextState ) ->
                    m ! [ message Changed, nextFrame <| LoadFromCache nextState ]

                Result.Ok ( m, Nothing ) ->
                    m ! []

                Result.Err s ->
                    { model | error = Just s } ! [ message StartSyncFiles ]

        RestoreFromCacheOrListFolder accountInfo ->
            let
                m =
                    { model
                        | accountId = Just accountInfo.accountId
                        , teamId = accountInfo.team |> Maybe.map .id
                        , cache = Nothing
                    }
            in
                case model.cache of
                    Just jsonString ->
                        -- delay in order to update the display
                        { m | state = Decoding 0.0 }
                            ! [ nextFrame <| LoadFromCache (JsonString jsonString) ]

                    Nothing ->
                        m ! [ message StartSyncFiles ]

        SaveToCache timestamp ->
            model ! [ saveFilesCache <| encode { model | state = FromCache timestamp } ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- DECODING


type CacheDecoderState
    = JsonString String
    | FileStrings
        { paths : List String
        , decoderState : Dropbox.Extras.SerializationState
        , total : Int
        , finalState : State
        }


decodePathBatch : CacheDecoderState -> Model -> Result String ( Model, Maybe CacheDecoderState )
decodePathBatch state model =
    case state of
        JsonString s ->
            case Decode.decodeString partialModelDecoder s of
                Result.Ok { files, state, accountId, teamId } ->
                    if ( model.accountId, model.teamId ) == ( accountId, teamId ) then
                        let
                            paths =
                                String.split ";" files
                        in
                            Result.Ok
                                ( model
                                , Just <|
                                    FileStrings
                                        { paths = paths
                                        , total = List.length paths
                                        , decoderState = Nothing
                                        , finalState = state
                                        }
                                )
                    else
                        Result.Err "Invalid cache; re-syncing…"

                Result.Err _ ->
                    -- The err is too long; displaying it hangs the browser
                    Result.Err "Coudn't read the cache; re-syncing…"

        FileStrings ({ paths, decoderState, total, finalState } as state) ->
            if List.isEmpty paths then
                Result.Ok ( { model | state = finalState }, Nothing )
            else
                let
                    batchSize =
                        1000

                    ( files, ds2 ) =
                        paths
                            |> List.take batchSize
                            |> FileTree.addFromStrings model.files decoderState

                    remainingPaths =
                        List.drop batchSize paths

                    completion =
                        1.0 - (toFloat (List.length remainingPaths)) / toFloat (max 1 total)
                in
                    Result.Ok
                        ( { model | files = files, state = Decoding completion }
                        , Just <|
                            FileStrings <|
                                { state
                                    | paths = remainingPaths
                                    , decoderState = ds2
                                }
                        )



-- SERIALIZATION


encodeState : State -> Encode.Value
encodeState state =
    case state of
        FromCache timestamp ->
            timestamp |> Date.fromTime |> Date.toUtcIsoString |> Encode.string

        _ ->
            Encode.null


stateDecoder : Decoder State
stateDecoder =
    Decode.string
        |> Decode.andThen (Date.fromIsoString >> Decode.fromResult)
        |> Decode.map Date.toTime
        |> Decode.map FromCache


encodingVersion : Int
encodingVersion =
    1


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "version", Encode.int encodingVersion )
        , ( "files", FileTree.encode model.files )
        , ( "state", encodeState model.state )
        , ( "accountId", Encode.string <| Maybe.withDefault "" model.accountId )
        , ( "teamId", Encode.string <| Maybe.withDefault "" model.teamId )
        ]


decoder : Decoder Model
decoder =
    decodeRequireVersion encodingVersion <|
        Decode.map4
            (\f s id tid ->
                { init
                    | files = f
                    , state = s
                    , accountId = maybeToDefault "" id
                    , teamId = maybeToDefault "" tid
                }
            )
            (Decode.field "files" FileTree.decoder)
            (Decode.field "state" stateDecoder)
            (Decode.field "accountId" Decode.string)
            (Decode.field "teamId" Decode.string)


partialModelDecoder : Decoder { files : String, state : State, accountId : Maybe String, teamId : Maybe String }
partialModelDecoder =
    decodeRequireVersion encodingVersion <|
        Decode.map4
            (\f s id tid ->
                { files = f
                , state = s
                , accountId = maybeToDefault "" id
                , teamId = maybeToDefault "" tid
                }
            )
            (Decode.field "files" Decode.string)
            (Decode.field "state" stateDecoder)
            (Decode.field "accountId" Decode.string)
            (Decode.field "teamId" Decode.string)



-- DECODE EXTRAS


decodeRequireVersion : Int -> Decoder a -> Decoder a
decodeRequireVersion version decoder =
    Decode.field "version" Decode.int
        |> Decode.andThen
            (\v ->
                if v == version then
                    decoder
                else
                    Decode.fail <| "Unknown version " ++ toString v
            )
