module Update exposing (update)

import Dropbox
import Dropbox.AccountInfo exposing (..)
import Extras exposing (remove)
import FilesComponent exposing (ModelChangeMsg(..))
import Json.Decode
import Message exposing (..)
import Model exposing (..)
import Navigation
import Ports exposing (..)
import Result
import Task
import TreeMap exposing (renderFileTreeMap)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- initial load, with saved access token
        InitializeAccessToken accessToken ->
            case accessToken of
                Just str ->
                    case Json.Decode.decodeString Dropbox.decodeUserAuth str of
                        Result.Ok auth ->
                            { model | auth = Just auth, status = SigningIn }
                                ! [ Task.attempt SetAccountInfo <| getCurrentAccount auth ]

                        Result.Err err ->
                            { model | errors = err :: model.errors } ! []

                Nothing ->
                    model ! []

        -- on OAuth callback
        AuthResponse (Dropbox.AuthorizeOk auth) ->
            { model | auth = Just auth.userAuth, status = SignedIn }
                ! [ Task.attempt SetAccountInfo <| getCurrentAccount auth.userAuth
                  , storeAccessToken <| Dropbox.encodeUserAuth auth.userAuth
                  , clearLocationHash model
                  ]

        AuthResponse err ->
            { model
                | auth = Nothing
                , errors = toString err :: model.errors
                , status = SignedOut
            }
                ! []

        -- user clicks sign in. redirect to OAuth
        SignIn ->
            model
                ! [ Dropbox.authorize
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

        -- user clicks sign out
        SignOut ->
            clearAccountFields model
                ! [ removeAccountInfo ()
                  , clearLocationHash model
                  ]

        SetAccountInfo (Result.Ok accountInfo) ->
            let
                newModel =
                    { model | accountInfo = Just accountInfo, status = SignedIn }
            in
                if FilesComponent.isEmpty model.files then
                    update (restoreOrSyncFiles accountInfo) newModel
                else
                    newModel ! []

        SetAccountInfo (Result.Err err) ->
            { model | errors = toString err :: model.errors } ! []

        FilesMessage filesMsg ->
            let
                ( m1, cmd ) =
                    updateFilesModel filesMsg model

                m =
                    combineErrors m1
            in
                case filesMsg of
                    FilesComponent.ModelChange Changed ->
                        let
                            ( m2, cmd2 ) =
                                update RenderFileTreeMap m
                        in
                            m2 ! [ cmd, cmd2 ]

                    FilesComponent.ModelChange Cleared ->
                        { m | path = "/" } ! [ cmd ]

                    _ ->
                        ( m, cmd )

        RenderFileTreeMap ->
            model
                ! [ Model.subtree model |> renderFileTreeMap model.depth ]

        -- view commands
        DismissMessageView n ->
            -- closing message by index instead of serial number could be
            -- a race condition, but probably less likely than the display
            -- updating between userremove : Int -> List a -> List a intention and action
            { model | errors = remove n model.errors } ! []

        SetFocus p ->
            update RenderFileTreeMap { model | path = p }

        SetSortOrder ord ->
            { model | order = ord } ! []

        SetTreeDepth n ->
            { model | depth = n } ! []


updateFilesModel : FilesComponent.Msg -> Model -> ( Model, Cmd Msg )
updateFilesModel msg model =
    case model.auth of
        Just auth ->
            let
                ( files, cmd ) =
                    FilesComponent.update auth msg model.files
            in
                { model | files = files } ! [ Cmd.map FilesMessage cmd ]

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
