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
        , encode
        , decoder
        , encodeStatus
        , statusDecoder
        )

{-|


## The synced file state and the syncronization status.
-}

import AccountInfo exposing (AccountInfo)
import CmdExtras exposing (..)
import Date
import Date.Extra as Date
import Dropbox exposing (..)
import DropboxExtras exposing (listFolderToContinueError)
import Extras exposing (maybeToDefault)
import FileTree exposing (FileTree)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import Ports exposing (saveFilesCache)
import Task
import Time exposing (Time)


-- MODEL


type alias Model =
    { files : FileTree
    , status : Status
    , error : Maybe String
    , accountId : Maybe String
    , teamId : Maybe String
    , cache : Maybe String
    }


type Status
    = Decoding
    | FromCache Time
    | Started
    | Syncing { entries : Int, requests : Int }
    | Synced { entries : Int, requests : Int }
    | Unsynced


init : Model
init =
    { files = FileTree.empty
    , status = Unsynced
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

        FromCache _ ->
            1.0

        Synced _ ->
            1.0

        _ ->
            0


isSyncing : Model -> Bool
isSyncing model =
    case model.status of
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
    | RestoreFromCache
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
                { model | files = FileTree.empty, status = Started }
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
                                , status = nextStatus model.status data
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
                        | status = Synced (syncStats model.status)
                        , error = Just <| toString err
                    }
                        ! []

        RestoreFromCache ->
            case model.cache |> Maybe.map (Decode.decodeString decoder) of
                Just (Result.Ok m) ->
                    if ( model.accountId, model.teamId ) == ( m.accountId, m.teamId ) then
                        m ! [ message Changed ]
                    else
                        update auth ListFolder { model | cache = Nothing, error = Just "Invalid cache; re-syncing…" }

                Just (Result.Err _) ->
                    -- The err is too long; displaying it hangs the browser
                    update auth
                        ListFolder
                        { model
                            | cache = Nothing
                            , error = Just "Coudn't read the cache; re-syncing…"
                        }

                Nothing ->
                    update auth ListFolder { model | cache = Nothing }

        RestoreFromCacheOrListFolder accountInfo ->
            let
                m =
                    { model
                        | accountId = Just accountInfo.accountId
                        , teamId = accountInfo.team |> Maybe.map .id
                    }
            in
                case m.cache of
                    Just _ ->
                        -- delay, in order to update the display
                        { m | status = Decoding } ! [ nextFrame RestoreFromCache ]

                    Nothing ->
                        update auth ListFolder m

        SaveToCache timestamp ->
            model ! [ saveFilesCache <| encode { model | status = FromCache timestamp } ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- SERIALIZATION


encodeStatus : Status -> Encode.Value
encodeStatus status =
    case status of
        FromCache timestamp ->
            timestamp |> Date.fromTime |> Date.toUtcIsoString |> Encode.string

        _ ->
            Encode.null


statusDecoder : Decoder Status
statusDecoder =
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
        , ( "status", encodeStatus model.status )
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
                    , status = s
                    , accountId = maybeToDefault "" id
                    , teamId = maybeToDefault "" tid
                }
            )
            (Decode.field "files" FileTree.decoder)
            (Decode.field "status" statusDecoder)
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
