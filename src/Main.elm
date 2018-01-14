port module Main exposing (..)

import BeautifulExample
import Color
import Dict
import Dropbox
import FileEntry exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Navigation
import Regex
import Round


main : Program Never Model (Dropbox.Msg Msg)
main =
    Dropbox.program
        { init = \location -> ( init location, Cmd.none )
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
    , githubUrl = Nothing
    , documentationUrl = Nothing
    }



-- model


type alias Model =
    { location : Navigation.Location
    , auth : Maybe Dropbox.UserAuth
    , clientId : String
    , debug : Maybe String
    , fileTree : FileTree
    , loadingTree : Bool
    , loadedEntryCount : Int
    }


init : Navigation.Location -> Model
init location =
    { location = location
    , auth = Nothing
    , clientId = ""
    , debug = Nothing
    , fileTree = emptyFileTree "/"
    , loadingTree = False
    , loadedEntryCount = 0
    }



-- messages


type Msg
    = SignIn
    | ClientID String
    | AuthResponse Dropbox.AuthorizeResult
    | ListFiles
    | FileList (List FileEntry) Bool
    | FileListError


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        AuthResponse (Dropbox.AuthorizeOk auth) ->
            { model | auth = Just auth.userAuth } ! []

        AuthResponse _ ->
            { model | auth = Nothing } ! []

        ClientID clientId ->
            { model | clientId = clientId } ! []

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
                , fileTree = emptyFileTree "/"
                , loadedEntryCount = 0
                , loadingTree = True
            }
                ! [ cmd ]

        FileList entries loading ->
            { model
                | fileTree = addFileEntries entries model.fileTree
                , loadingTree = loading
                , loadedEntryCount = model.loadedEntryCount + List.length entries
            }
                ! []

        FileListError ->
            { model
                | debug = Just "Error listing files"
                , loadingTree = False
            }
                ! []



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dropboxClientID ClientID
        , fileList <| uncurry FileList
        ]


port dropboxClientID : (String -> msg) -> Sub msg


port listFiles : String -> Cmd msg


port fileList : (( List FileEntry, Bool ) -> msg) -> Sub msg


port fileListError : (() -> msg) -> Sub msg



-- view


view : Model -> Html Msg
view model =
    div []
        [ Html.button
            [ Html.Events.onClick SignIn ]
            [ Html.text (model.auth |> Maybe.map (\_ -> "Sign out") |> Maybe.withDefault "Sign into Dropbox") ]
        , if model.loadingTree then
            div [] []
          else
            Html.button
                [ Html.Events.onClick ListFiles ]
                [ Html.text "Sync" ]
        , div []
            [ text
                (if model.loadingTree then
                    "Syncing (" ++ toString model.loadedEntryCount ++ " entries synced)"
                 else
                    toString model.loadedEntryCount ++ " entries synced"
                )
            ]
        , div [] [ model.debug |> Maybe.withDefault "" |> text ]
        , treeView_ 2 model.fileTree
        ]


treeView_ : number -> FileTree -> Html msg
treeView_ depth tree =
    -- let
    --     _ =
    --         Debug.log "tree" tree
    -- in
    treeView depth tree


treeView : number -> FileTree -> Html msg
treeView depth tree =
    let
        label entry size =
            entry.path ++ " (" ++ humanize size ++ ")"

        childViews children =
            Html.ul []
                (Dict.values children
                    |> List.sortBy (itemEntry >> .path >> String.toUpper)
                    |> List.map
                        (\t -> Html.li [] [ treeView (depth - 1) t ])
                )
    in
    case tree of
        File entry ->
            div [] [ text <| label entry (Maybe.withDefault 0 entry.size) ]

        Dir entry size children ->
            div []
                [ text <| label entry size
                , if depth > 0 then
                    childViews children
                  else
                    div [] []
                ]



-- utilities


extractAccessToken : Dropbox.UserAuth -> Maybe String
extractAccessToken auth =
    -- TODO extract from JSON instead?
    auth |> toString |> firstMatch (Regex.regex "Bearer \"(.+)\"")


firstMatch : Regex.Regex -> String -> Maybe String
firstMatch re s =
    case Regex.find Regex.All re s of
        { submatches } :: _ ->
            case submatches of
                [ s ] ->
                    s

                _ ->
                    Nothing

        _ ->
            Nothing


humanize : Int -> String
humanize n =
    case List.filter (\( s, _ ) -> toFloat n > s) [ ( 1.0e12, "T" ), ( 1.0e9, "G" ), ( 1.0e6, "M" ), ( 1.0e3, "K" ) ] of
        ( s, unit ) :: _ ->
            (toFloat n / s |> Round.round 1) ++ unit

        _ ->
            toString n ++ " bytes"


fileBase : String -> String
fileBase path =
    path |> String.split "/" |> List.foldl (\a b -> b) path
