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
    = Decoding
    | FromCache Time
    | Started
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



--- STATUS


nextState : State -> { c | entries : List a, hasMore : Bool } -> State
nextState state { entries, hasMore } =
    let
        data =
            syncStats state
    in
        { entries = List.length entries + data.entries, requests = 1 + data.requests }
            |> if hasMore then
                Syncing
               else
                Synced


syncStats : State -> { entries : Int, requests : Int }
syncStats state =
    case state of
        Syncing data ->
            data

        _ ->
            { entries = 0, requests = 0 }


syncFraction : Model -> Float
syncFraction model =
    case model.state of
        Syncing { requests } ->
            let
                f =
                    toFloat requests
            in
                f / (f + 1.0)

        FromCache _ ->
            1.0

        Synced _ ->
            1.0

        _ ->
            0


isSyncing : Model -> Bool
isSyncing model =
    case model.state of
        Decoding ->
            True

        Started ->
            True

        Syncing _ ->
            True

        _ ->
            False



-- MESSAGES


type Msg
    = Changed
    | Cleared
    | ListFolder
    | ReceiveListFolderResponse (Result ListFolderContinueError ListFolderResponse)
    | RestoreFromCacheOrListFolder AccountInfo
    | LoadCache CacheDecoderState
    | SaveToCache Time



-- UPDATE


update : Dropbox.UserAuth -> Msg -> Model -> ( Model, Cmd Msg )
update auth msg model =
    case msg of
        Changed ->
            ( model, Cmd.none )

        Cleared ->
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
                { model | files = FileTree.empty, state = Started }
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
                                , state = nextState model.state data
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
                        | state = Synced (syncStats model.state)
                        , error = Just <| toString err
                    }
                        ! []

        LoadCache state ->
            case updateDecoder state model of
                Result.Ok ( m, Just s ) ->
                    m ! [ message Changed, message <| LoadCache s ]

                Result.Ok ( m, Nothing ) ->
                    m ! []

                Result.Err s ->
                    { model | error = Just s } ! [ message <| ListFolder ]

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
                        { m | state = Decoding }
                            ! [ nextFrame <| LoadCache (JsonString jsonString) ]

                    Nothing ->
                        update auth ListFolder m

        SaveToCache timestamp ->
            model ! [ saveFilesCache <| encode { model | state = FromCache timestamp } ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- DECODING


type CacheDecoderState
    = JsonString String
    | FileStrings State (List String)


updateDecoder : CacheDecoderState -> Model -> Result String ( Model, Maybe CacheDecoderState )
updateDecoder state model =
    case state of
        JsonString s ->
            case Decode.decodeString partialModelDecoder s of
                Result.Ok { files, state, accountId, teamId } ->
                    if ( model.accountId, model.teamId ) == ( accountId, teamId ) then
                        Result.Ok ( model, Just <| FileStrings state <| String.split ";" files )
                    else
                        Result.Err "Invalid cache; re-syncing…"

                Result.Err _ ->
                    -- The err is too long; displaying it hangs the browser
                    Result.Err "Coudn't read the cache; re-syncing…"

        FileStrings state [] ->
            Result.Ok ( { model | state = state }, Nothing )

        FileStrings state paths ->
            let
                n =
                    100

                files =
                    paths
                        |> List.take n
                        |> List.map Dropbox.Extras.decodeString
                        |> flip FileTree.addEntries model.files
            in
                Result.Ok ( { model | files = files }, Just <| FileStrings state <| List.drop n paths )



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
