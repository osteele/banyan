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

import CmdExtras exposing (..)
import Date
import Date.Extra as Date
import Dropbox exposing (..)
import DropboxExtras exposing (listFolderToContinueError)
import FileTree exposing (FileTree)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import ListFolder exposing (..)
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
    | ListFolder
    | ReceiveListFolderResponse (Result ListFolderContinueError ListFolderResponse)
    | RestoreFromCacheOrListFolder
    | RestoreFromCache
    | SaveToCache Time



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
                                Dropbox.listFolderContinue auth { cursor = cursor }
                                    |> Task.attempt ReceiveListFolderResponse
                            else
                                Task.perform SaveToCache Time.now
                    in
                        m ! [ cmd, message Changed ]

                Result.Err err ->
                    { model
                        | status = Synced (syncStats model.status)
                        , errorMessage = Just <| toString err
                    }
                        ! []

        RestoreFromCache ->
            case model.cache |> Maybe.map (Decode.decodeString decoder) of
                Just (Result.Ok m) ->
                    m ! [ message Changed ]

                Just (Result.Err err) ->
                    update auth
                        ListFolder
                        { model
                            | cache = Nothing
                            , errorMessage = Just <| err
                        }

                Nothing ->
                    update auth ListFolder { model | cache = Nothing }

        RestoreFromCacheOrListFolder ->
            case model.cache of
                Just _ ->
                    -- delay, in order to update the display
                    { model | status = Decoding } ! [ nextFrame RestoreFromCache ]

                Nothing ->
                    update auth ListFolder model

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
encode { files, status } =
    Encode.object
        [ ( "version", Encode.int encodingVersion )
        , ( "files", FileTree.encode files )
        , ( "status", encodeStatus status )
        ]


decoder : Decoder Model
decoder =
    decodeRequireVersion encodingVersion <|
        Decode.map2 (\f s -> { init | files = f, status = s })
            (Decode.field "files" FileTree.decoder)
            (Decode.field "status" statusDecoder)



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
