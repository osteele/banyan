module Main exposing (..)

--exposing (..)
-- exposing (..)

import Dropbox
import FileEntry
import FileTree
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
    { accessToken : Maybe String
    , clientId : String
    }


main : Program Flags Model (Dropbox.Msg Msg)
main =
    Dropbox.programWithFlags
        { init =
            \flags location -> init flags.clientId location ! [ initialCmd flags ]
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
                        , storeAccessToken <| Just token
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
                ! [ storeAccessToken Nothing
                  , clearLocationHash model
                  ]

        SetAccountInfo info ->
            update ListFiles { model | accountInfo = Just info }

        ListFiles ->
            { model
                | debug = Nothing
                , fileTree = FileTree.empty
                , loadedEntryCount = 0
                , loadingTree = True
                , path = "/"
            }
                ! [ model.auth
                        |> Maybe.andThen extractAccessToken
                        |> Maybe.map listFiles
                        |> Maybe.withDefault Cmd.none
                  ]

        FileList entries loading ->
            let
                model2 =
                    { model
                        | fileTree = FileTree.addEntries entries model.fileTree
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

        Focus p ->
            update RenderFileTreeMap { model | path = p }

        TreeDepth n ->
            { model | depth = n } ! []

        RenderFileTreeMap ->
            model
                ! [ Model.subtree model
                        |> fileTreeMap 1
                  ]


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
        [ receiveFileList <| uncurry FileList
        , receiveAccountInfo SetAccountInfo
        , setPath Focus
        ]



-- utilities


extractAccessToken : Dropbox.UserAuth -> Maybe String
extractAccessToken auth =
    -- TODO extract from JSON instead?
    auth |> toString |> firstMatch (Regex.regex "Bearer \"(.+)\"")
