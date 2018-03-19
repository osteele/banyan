module Main exposing (..)

import CmdExtras exposing (..)
import Dropbox
import DropboxExtras exposing (extractAccessToken)
import FilesComponent
import Message exposing (..)
import Model exposing (..)
import Navigation
import Ports exposing (..)
import TreeMap exposing (renderFileTreeMap)
import View exposing (..)


type alias Flags =
    { accessToken : Maybe String
    , clientId : String
    , files : Maybe String
    }


main : Program Flags Model (Dropbox.Msg Msg)
main =
    Dropbox.programWithFlags
        { init =
            \flags location ->
                Model.init flags location ! [ initialCmd flags ]
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onAuth = AuthResponse
        }


initialCmd : Flags -> Cmd Msg
initialCmd flags =
    message <| AccessToken flags.accessToken



-- MESSAGES


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- account
        AccessToken accessToken ->
            case accessToken of
                Just tokenString ->
                    { model
                        | auth = Just <| Dropbox.authorizationFromAccessToken tokenString
                        , status = SigningIn
                    }
                        ! [ getAccountInfo tokenString ]

                Nothing ->
                    model ! []

        AuthResponse (Dropbox.AuthorizeOk auth) ->
            { model | auth = Just auth.userAuth, status = SignedIn }
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
            -- TODO display the error message
            { model | auth = Nothing, status = SignedOut } ! []

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

        SignOut ->
            clearAccountFields model
                ! [ removeAccountInfo ()
                  , clearLocationHash model
                  ]

        SetAccountInfo info ->
            let
                m =
                    { model | accountInfo = Just info, status = SignedIn }
            in
                if FilesComponent.isEmpty model.files then
                    update restoreOrSyncFiles m
                else
                    m ! []

        FilesMessage fmsg ->
            let
                ( m, cmd ) =
                    updateFilesModel fmsg model
            in
                case fmsg of
                    FilesComponent.Changed ->
                        let
                            ( m2, cmd2 ) =
                                update RenderFileTreeMap m
                        in
                            m2 ! [ cmd, cmd2 ]

                    FilesComponent.ListFolder ->
                        { m | path = "/" } ! [ cmd ]

                    FilesComponent.RestoreFromCacheOrListFolder ->
                        { m | path = "/" } ! [ cmd ]

                    _ ->
                        ( m, cmd )

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveAccountInfo SetAccountInfo
        , setPath Focus
        , Sub.map FilesMessage <| FilesComponent.subscriptions model.files
        ]
