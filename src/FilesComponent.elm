module FilesComponent
    exposing
        ( Model
        , Msg(..)
        , State(..)
        , fromCache
        , init
        , isEmpty
        , isLoading
        , subscriptions
        , completion
        , progress
        , update
        , encode
        , decoder
        , encodeState
        , stateDecoder
        )

{-|


## This component maintains a model of the Dropbox files.

It creates its model from Dropbox API or from a local cache, and saves it to
a cache.

This is a component rather than a data structure, because it uses Commands to
read and store its state.

-}

import CmdExtras exposing (..)
import Date
import Date.Extra as Date
import Dropbox exposing (..)
import Dropbox.AccountInfo exposing (AccountInfo)
import Dropbox.Extras exposing (listFolderToContinueError)
import Dropbox.FileTree as FileTree exposing (FileTree)
import Extras exposing (maybeToDefault, quantify, toStringWithCommas)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import Ports exposing (saveFilesCache)
import Task
import Time exposing (Time)


-- MODEL


{-| A Dropbox FileTree, and the values necessary to sync it from a Dropbox
account.

    - files: The Dropbox folder tree
    - state: The synchronization or cache deserialization state
    - error: The last error.
    - accountId, teamId: Used to insure the cache matches the account.
    - cache: A cache that is consulted once the user is authorized.

-}
type alias Model =
    { files : FileTree
    , state : State
    , error : Maybe String
    , accountId : Maybe String
    , teamId : Maybe String
    , cache : Maybe String
    }


type State
    = Decoding CacheDecoderState
    | FromCache Time
    | StartedSync
    | Syncing { entryCount : Int, requestCount : Int }
    | Synced { entryCount : Int, requestCount : Int }
    | Unsynced



-- MESSAGES


type Msg
    = Changed
    | Cleared
    | StartSyncFiles
    | ReceiveListFolderResponse (Result ListFolderContinueError ListFolderResponse)
    | RestoreFromCacheOrListFolder AccountInfo
    | LoadFromCache CacheDecoderState
    | SaveToCache Time



-- INITIALIZATION


init : Model
init =
    { files = FileTree.empty
    , state = Unsynced
    , error = Nothing
    , accountId = Nothing
    , teamId = Nothing
    , cache = Nothing
    }


{-| Initialize a model, with a cache that is consulted once the
RestoreFromCacheOrListFolder message is sent.
-}
fromCache : Maybe String -> Model
fromCache c =
    { init | cache = c }



--- STATE QUERIES


{-| Return a float between 0.0 and 1.0, that indicates the completion progress
of a file list or cache decode operation.

The completion of a file list is an estimate with exponential backoff. See
<https://github.com/osteele/banyan/issues/9>.

-}
completion : Model -> Float
completion model =
    case model.state of
        Decoding state ->
            decodingCompletion state

        FromCache _ ->
            1.0

        Syncing { requestCount } ->
            toFloat requestCount |> \n -> n / (n + 1)

        Synced _ ->
            1.0

        _ ->
            0


{-| Determine if the set of files and folders is empty.
-}
isEmpty : Model -> Bool
isEmpty =
    .files >> FileTree.isEmpty


{-| Determine if the model is still loading from Dropbox or from the cache.
-}
isLoading : Model -> Bool
isLoading model =
    case model.state of
        Decoding _ ->
            True

        StartedSync ->
            True

        Syncing _ ->
            True

        _ ->
            False


{-| Return a string description of the loading or synchronization progress.
-}
progress : Model -> String
progress { state } =
    case state of
        Unsynced ->
            "Unsyced"

        StartedSync ->
            "Starting sync…"

        Syncing { entryCount, requestCount } ->
            String.join "" <|
                [ "Loaded "
                , toStringWithCommas entryCount
                , " entries in "
                , quantify "request" requestCount
                , "…"
                ]

        Synced { entryCount, requestCount } ->
            String.join "" <|
                [ "Loaded "
                , toStringWithCommas entryCount
                , " entries in "
                , quantify "request" requestCount
                , "."
                ]

        Decoding s ->
            case s of
                FileStrings { addedPathCount, totalPathCount } ->
                    String.join "" <|
                        [ "Loaded "
                        , toStringWithCommas addedPathCount
                        , " of "
                        , quantify " file" totalPathCount
                        , " from the cache…"
                        ]

                _ ->
                    "Loading from the cache…"

        FromCache timestamp ->
            String.join "" <|
                [ "Cached at "
                , Date.toFormattedString "h:mm a on EEEE, MMMM d, y" <| Date.fromTime timestamp
                , ". "
                ]



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
                ( m, msg ) =
                    startListFolder auth model
            in
                m ! [ message Changed, msg ]

        ReceiveListFolderResponse result ->
            let
                ( m, msg ) =
                    updateFromListFolderResponse auth model result
            in
                m ! [ message Changed, msg ]

        LoadFromCache decoderState ->
            case decodePathBatch model decoderState of
                CacheDecoderProgress m nextState ->
                    { m | state = Decoding nextState }
                        ! [ message Changed, nextFrame <| LoadFromCache nextState ]

                CacheDecoderComplete m finalState ->
                    { m | state = finalState } ! []

                CacheDecoderError s ->
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
                        let
                            state =
                                JsonString jsonString
                        in
                            -- delay in order to update the display
                            { m | state = Decoding state }
                                ! [ nextFrame <| LoadFromCache state ]

                    Nothing ->
                        m ! [ message StartSyncFiles ]

        SaveToCache timestamp ->
            model ! [ saveFilesCache <| encode { model | state = FromCache timestamp } ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- LIST FOLDERS


startListFolder : Dropbox.UserAuth -> Model -> ( Model, Cmd Msg )
startListFolder auth model =
    let
        listFolder =
            Task.mapError listFolderToContinueError <|
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
              , Task.attempt ReceiveListFolderResponse listFolder
              ]


updateFromListFolderResponse : Dropbox.UserAuth -> Model -> Result ListFolderContinueError ListFolderResponse -> ( Model, Cmd Msg )
updateFromListFolderResponse auth model result =
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
                m ! [ cmd ]

        Result.Err err ->
            { model
                | state = nextSyncState model.state { entries = [], hasMore = False }
                , error = Just <| toString err
            }
                ! []


nextSyncState : State -> { a | entries : List Metadata, hasMore : Bool } -> State
nextSyncState state { entries, hasMore } =
    let
        { entryCount, requestCount } =
            case state of
                Syncing data ->
                    data

                _ ->
                    { entryCount = 0, requestCount = 0 }
    in
        { entryCount = entryCount + List.length entries
        , requestCount = requestCount + 1
        }
            |> if hasMore then
                Syncing
               else
                Synced



-- DECODING


type CacheDecoderState
    = JsonString String
    | FileStrings
        { paths : List String
        , decoderState : Dropbox.Extras.SerializationState
        , addedPathCount : Int
        , totalPathCount : Int
        , finalState : State
        }


type CacheDecoderResult
    = CacheDecoderProgress Model CacheDecoderState
    | CacheDecoderComplete Model State
    | CacheDecoderError String


decodePathBatch : Model -> CacheDecoderState -> CacheDecoderResult
decodePathBatch model state =
    case state of
        JsonString s ->
            case Decode.decodeString partialModelDecoder s of
                Result.Ok { files, state, accountId, teamId } ->
                    if ( model.accountId, model.teamId ) == ( accountId, teamId ) then
                        let
                            paths =
                                String.split ";" files
                        in
                            CacheDecoderProgress model <|
                                FileStrings
                                    { paths = paths
                                    , addedPathCount = 0
                                    , totalPathCount = List.length paths
                                    , decoderState = Nothing
                                    , finalState = state
                                    }
                    else
                        CacheDecoderError "Invalid cache; re-syncing…"

                Result.Err _ ->
                    -- The err is too long; displaying it hangs the browser
                    CacheDecoderError "Coudn't read the cache; re-syncing…"

        FileStrings ({ paths, decoderState, finalState } as state) ->
            if List.isEmpty paths then
                CacheDecoderComplete model finalState
            else
                let
                    batchSize =
                        1000

                    batch =
                        List.take batchSize paths

                    ( files, newDecoderState ) =
                        batch
                            |> FileTree.addFromStrings model.files decoderState

                    nextState =
                        FileStrings
                            { state
                                | paths = List.drop batchSize paths
                                , decoderState = newDecoderState
                                , addedPathCount = state.addedPathCount + List.length batch
                            }
                in
                    CacheDecoderProgress { model | files = files } nextState


decodingCompletion : CacheDecoderState -> Float
decodingCompletion state =
    case state of
        JsonString _ ->
            0.0

        FileStrings { addedPathCount, totalPathCount } ->
            (toFloat addedPathCount) / toFloat (max 1 totalPathCount)



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
