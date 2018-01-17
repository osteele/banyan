module Main exposing (..)

import BeautifulExample
import Color
import Dropbox
import FileEntry exposing (..)
import Message exposing (..)
import Model exposing (..)
import Navigation
import Ports exposing (..)
import Regex
import Task
import TreeMap exposing (fileTreeMap)
import Utils exposing (..)
import View exposing (..)


type alias Flags =
    { accessToken : Maybe String }


main : Program Flags Model (Dropbox.Msg Msg)
main =
    Dropbox.programWithFlags
        { init =
            \flags location ->
                let
                    cmd =
                        AccessToken flags.accessToken
                            |> Task.succeed
                            |> Task.perform identity
                in
                init location ! [ cmd ]
        , update = update
        , subscriptions = subscriptions
        , view = \model -> BeautifulExample.view config (view model)
        , onAuth = AuthResponse
        }


config : BeautifulExample.Config
config =
    { title = "Banyan"
    , details = Just "Dropbox file size browser."
    , color = Just Color.blue
    , maxWidth = 800
    , githubUrl = Just "https://github.com/osteele/banyan"
    , documentationUrl = Nothing
    }



-- messages


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        AuthResponse (Dropbox.AuthorizeOk auth) ->
            { model | auth = Just auth.userAuth }
                ! (case auth.userAuth |> extractAccessToken of
                    Just token ->
                        [ getAccountInfo token
                        , storeAccessToken <| Just token
                        , clearLocationHash model
                        ]

                    Nothing ->
                        []
                  )

        AuthResponse _ ->
            { model | auth = Nothing } ! []

        ClientID clientId ->
            { model | clientId = clientId } ! []

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
                ! [ storeAccessToken Nothing
                  , clearLocationHash model
                  ]

        SetAccountInfo info ->
            update ListFiles { model | accountInfo = Just info }

        AccessToken accessToken ->
            case accessToken of
                Just tokenString ->
                    let
                        token =
                            Dropbox.authorizationFromAccessToken tokenString
                    in
                    { model | auth = Just token } ! [ getAccountInfo tokenString ]

                _ ->
                    model ! []

        ListFiles ->
            let
                cmd =
                    case model.auth |> Maybe.andThen extractAccessToken of
                        Just token ->
                            listFiles token

                        Nothing ->
                            Cmd.none
            in
            { model
                | debug = Nothing
                , fileTree = FileEntry.empty
                , loadedEntryCount = 0
                , loadingTree = True
                , path = "/"
            }
                ! [ cmd ]

        FileList entries loading ->
            let
                model2 =
                    { model
                        | fileTree = addEntries entries model.fileTree
                        , loadingTree = loading
                        , loadedEntryCount = model.loadedEntryCount + List.length entries
                        , requestCount = model.requestCount + 1
                    }
            in
            update RenderFileTreeMap model2

        FileListError ->
            { model
                | debug = Just "Error listing files"
                , loadingTree = False
            }
                ! []

        Focus path ->
            update RenderFileTreeMap { model | path = path }

        TreeDepth n ->
            update RenderFileTreeMap { model | depth = n }

        RenderFileTreeMap ->
            model
                ! [ getSubtree model.path model.fileTree
                        |> Maybe.map (fileTreeMap 1)
                        |> Maybe.withDefault Cmd.none
                  ]


clearLocationHash : Model -> Cmd msg
clearLocationHash model =
    let
        location =
            model.location

        url =
            String.join "" [ location.protocol, "//", location.host, location.pathname ]
    in
    Navigation.modifyUrl url



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dropboxClientID ClientID
        , fileList <| uncurry FileList
        , setAccountInfo SetAccountInfo
        ]



-- utilities


extractAccessToken : Dropbox.UserAuth -> Maybe String
extractAccessToken auth =
    -- TODO extract from JSON instead?
    auth |> toString |> firstMatch (Regex.regex "Bearer \"(.+)\"")
