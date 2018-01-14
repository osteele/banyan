port module Main exposing (..)

import BeautifulExample
import Color
import Dict
import Dropbox
import FileEntry exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Navigation
import Regex
import Utils exposing (..)


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
    , githubUrl = Just "https://github.com/osteele/banyan"
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
    , accountInfo : Maybe AccountInfo
    , path : String
    }


init : Navigation.Location -> Model
init location =
    { location = location
    , auth = Nothing
    , clientId = ""
    , debug = Nothing
    , fileTree = FileEntry.empty
    , loadingTree = False
    , loadedEntryCount = 0
    , accountInfo = Nothing
    , path = "/"
    }



-- messages


type Msg
    = SignIn
    | ClientID String
    | AuthResponse Dropbox.AuthorizeResult
    | SetAccountInfo AccountInfo
    | ListFiles
    | FileList (List FileEntry) Bool
    | FileListError
    | Focus String


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        AuthResponse (Dropbox.AuthorizeOk auth) ->
            { model | auth = Just auth.userAuth }
                ! [ case auth.userAuth |> extractAccessToken of
                        Just token ->
                            getAccountInfo token

                        Nothing ->
                            Cmd.none
                  ]

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

        SetAccountInfo info ->
            update ListFiles { model | accountInfo = Just info }

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
            { model
                | fileTree = addEntries entries model.fileTree
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

        Focus path ->
            { model | path = path } ! []



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dropboxClientID ClientID
        , fileList <| uncurry FileList
        , setAccountInfo SetAccountInfo
        ]


port dropboxClientID : (String -> msg) -> Sub msg


port listFiles : String -> Cmd msg


port fileList : (( List FileEntry, Bool ) -> msg) -> Sub msg


port fileListError : (() -> msg) -> Sub msg


port getAccountInfo : String -> Cmd msg


type alias UserInfo =
    { abbreviated_name : String
    , display_name : String
    , familiar_name : String
    , given_name : String
    , surname : String
    }


type alias AccountInfo =
    { name : UserInfo
    , teamName : String
    }


port setAccountInfo : (AccountInfo -> msg) -> Sub msg



-- view


view : Model -> Html Msg
view model =
    let
        _ =
            if False then
                Debug.log "tree" model.fileTree
            else
                model.fileTree
    in
    div []
        [ ifDiv (model.accountInfo |> Maybe.map (always True) |> Maybe.withDefault False) <|
            div [] [ text (model.accountInfo |> Maybe.map .name |> Maybe.map .display_name |> Maybe.withDefault "") ]
        , Html.button
            [ Html.Events.onClick SignIn ]
            [ Html.text (model.auth |> Maybe.map (\_ -> "Sign out") |> Maybe.withDefault "Sign into Dropbox") ]
        , ifDiv (not model.loadingTree) <|
            Html.button
                [ Html.Events.onClick ListFiles ]
                [ Html.text "Sync" ]
        , div []
            [ text
                (if model.loadingTree then
                    "Syncing… " ++ toString model.loadedEntryCount ++ " entries synced"
                 else
                    toString model.loadedEntryCount ++ " entries synced"
                )
            ]
        , div [] [ model.debug |> Maybe.withDefault "" |> text ]
        , ifDiv (FileEntry.isEmpty model.fileTree |> not) <|
            treeView (model.accountInfo |> Maybe.map .teamName |> Maybe.withDefault "Personal")
                fileViewDepth
            <|
                (getSubtree model.path model.fileTree |> Maybe.withDefault model.fileTree)
        ]


folderClass =
    cssClass "folder"


cssClass name =
    Html.Attributes.property "className" (Encode.string name)


ifDiv test html =
    if test then
        html
    else
        div [] []


fileViewDepth : Int
fileViewDepth =
    2


treeView : String -> Int -> FileTree -> Html Msg
treeView teamName depth tree =
    let
        breadcrumbs =
            Html.div
                [ cssClass "breadcrumb" ]
                (itemEntry tree
                    |> .path
                    |> String.split "/"
                    |> List.map
                        (\p ->
                            Html.span
                                [ Html.Events.onClick <| Focus "/"
                                , folderClass
                                ]
                                [ text <|
                                    (if p == "" then
                                        teamName
                                     else
                                        p
                                    )
                                        ++ " > "
                                ]
                        )
                )
    in
    Html.div
        []
        [ breadcrumbs
        , treeView_ depth tree
        ]


treeView_ : number -> FileTree -> Html Msg
treeView_ depth tree =
    let
        label entry size =
            let
                name =
                    entry.path |> takeFileName
            in
            name ++ " (" ++ humanize size ++ ")"

        childViews children =
            Html.ul []
                (Dict.values children
                    |> List.sortBy (itemEntry >> .path >> String.toUpper)
                    |> List.map
                        (\t -> Html.li [] [ treeView_ (depth - 1) t ])
                )
    in
    case tree of
        File entry ->
            Html.div
                []
                [ text <| label entry (Maybe.withDefault 0 entry.size) ]

        Dir entry size children ->
            Html.div
                []
                [ Html.a
                    [ Html.Events.onClick <| Focus entry.key
                    , folderClass
                    ]
                    [ text <| label entry size ]
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
