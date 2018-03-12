module Main exposing (..)

import Dropbox
import DropboxUtils exposing (extractAccessToken)
import FilesModel
import Message exposing (..)
import Model exposing (..)
import Navigation
import Ports exposing (..)
import Task
import TreeMap exposing (renderFileTreeMap)
import View exposing (..)


type alias Flags =
    { accessToken : Maybe String
    , clientId : String
    }


main : Program Flags Model (Dropbox.Msg Msg)
main =
    Dropbox.programWithFlags
        { init =
            \flags location ->
                Model.init flags.clientId location ! [ initialCmd flags ]
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onAuth = AuthResponse
        }


initialCmd : Flags -> Cmd Msg
initialCmd flags =
    AccessToken flags.accessToken
        |> Task.succeed
        |> Task.perform identity



-- messages


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        -- account
        AccessToken accessToken ->
            case accessToken of
                Just tokenString ->
                    { model
                        | auth = Just <| Dropbox.authorizationFromAccessToken tokenString
                    }
                        ! [ getAccountInfo tokenString ]

                Nothing ->
                    model ! []

        AuthResponse (Dropbox.AuthorizeOk auth) ->
            { model | auth = Just auth.userAuth }
                ! (case auth.userAuth |> extractAccessToken of
                    Just token ->
                        [ getAccountInfo token
                        , storeAccessToken token
                        , clearLocationHash model
                        ]

                    Nothing ->
                        []
                  )

        AuthResponse _ ->
            { model | auth = Nothing } ! []

        SignIn ->
            model
                ! [ --
                    Dropbox.authorize
                        { clientId = model.clientId
                        , state = Nothing
                        , requireRole = Nothing
                        , forceReapprove = False
                        , disableSignup = False
                        , locale = Nothing
                        , forceReauthentication = False
                        }
                        model.location
                  ]

        SignOut ->
            clearAccountFields model
                ! [ removeAccountInfo ()
                  , clearLocationHash model
                  ]

        SetAccountInfo info ->
            update SyncFiles { model | accountInfo = Just info }

        -- list files
        SyncFiles ->
            let
                ( model_, cmd ) =
                    updateFileList msg model
            in
                { model_ | path = "/" } ! [ cmd ]

        ReceiveListFolderResponse _ ->
            let
                ( model_, cmd ) =
                    updateFileList msg model

                ( model__, cmd_ ) =
                    update RenderFileTreeMap model_
            in
                model__ ! [ cmd, cmd_ ]

        RenderFileTreeMap ->
            model
                ! [ Model.subtree model |> renderFileTreeMap model.depth ]

        -- view commands
        Focus p ->
            update RenderFileTreeMap { model | path = p }

        SortOrder ord ->
            { model | order = ord } ! []

        TreeDepth n ->
            { model | depth = n } ! []


updateFileList : Msg -> Model -> ( Model, Cmd msg )
updateFileList msg model =
    case model.auth of
        Just auth ->
            let
                ( files, cmd ) =
                    FilesModel.update auth msg model.files
            in
                { model | files = files } ! [ cmd ]

        Nothing ->
            model ! []


{-| Remove the hash from the browser navigation URL.
-}
clearLocationHash : Model -> Cmd msg
clearLocationHash model =
    let
        location =
            model.location
    in
        Navigation.modifyUrl <|
            String.join ""
                [ location.protocol, "//", location.host, location.pathname ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveAccountInfo SetAccountInfo
        , setPath Focus
        , FilesModel.subscriptions model.files
        ]
