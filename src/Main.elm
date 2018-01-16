module Main exposing (..)

import BeautifulExample
import Color
import Data exposing (..)
import Dict
import Dropbox
import FileEntry exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Navigation
import Ports exposing (..)
import Regex
import Utils exposing (..)


main : Program Never Model (Dropbox.Msg Msg)
main =
    Dropbox.program
        { init = \location -> init location ! []
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
    , requestCount : Int
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
    , requestCount = 0
    , accountInfo = Nothing
    , path = "/"
    }


clearAccountFields : Model -> Model
clearAccountFields model =
    { model
        | auth = Nothing
        , accountInfo = Nothing
        , fileTree = FileEntry.empty
        , loadingTree = False
        , loadedEntryCount = 0
        , requestCount = 0
        , path = "/"
    }



-- messages


type Msg
    = SignIn
    | SignOut
    | ClientID String
    | AuthResponse Dropbox.AuthorizeResult
    | SetAccountInfo AccountInfo
    | ReceiveLocalStore String (Maybe String)
    | ListFiles
    | FileList (List FileEntry) Bool
    | FileListError
    | Focus String


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

        ReceiveLocalStore key value ->
            let
                _ =
                    Debug.log "receive" ( key, value )
            in
            case
                if key == accessTokenKey then
                    value
                else
                    Nothing
            of
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
            { model
                | fileTree = addEntries entries model.fileTree
                , loadingTree = loading
                , loadedEntryCount = model.loadedEntryCount + List.length entries
                , requestCount = model.requestCount + 1
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
        , receiveLocalStore <| uncurry ReceiveLocalStore
        ]



--- access tokens


accessTokenKey : String
accessTokenKey =
    "accessToken"


storeAccessToken : Maybe String -> Cmd msg
storeAccessToken token =
    setLocalStore ( accessTokenKey, token )


requestAccessToken : Cmd msg
requestAccessToken =
    getLocalStore accessTokenKey



-- view


view : Model -> Html Msg
view model =
    div []
        [ ifDiv (isSignedIn model) <|
            div [] [ text (model.accountInfo |> Maybe.map .name |> Maybe.map .display_name |> Maybe.withDefault "") ]
        , ifDiv (isSignedIn model) <|
            Html.button
                [ Html.Events.onClick <| SignOut ]
                [ Html.text <| "Sign out" ]
        , ifDiv (not <| isSignedIn model) <|
            Html.button
                [ Html.Events.onClick <| SignIn ]
                [ Html.text <| "Sign into Dropbox" ]
        , ifDiv (isSignedIn model && not model.loadingTree) <|
            Html.button
                [ Html.Events.onClick ListFiles ]
                [ Html.text "Sync" ]
        , div []
            [ text <|
                String.join ""
                    [ toStringWithCommas model.loadedEntryCount
                    , " entries totalling "
                    , humanize <| nodeSize model.fileTree
                    , " loaded in "
                    , toString model.requestCount
                    , " requests"
                    , if model.loadingTree then
                        "…"
                      else
                        "."
                    ]
            ]
        , div [] [ model.debug |> Maybe.withDefault "" |> text ]
        , ifDiv (FileEntry.isEmpty model.fileTree |> not) <|
            treeView (model.accountInfo |> Maybe.map .teamName |> Maybe.withDefault "Personal")
                fileViewDepth
            <|
                (getSubtree model.path model.fileTree |> Maybe.withDefault model.fileTree)
        ]


fileViewDepth : Int
fileViewDepth =
    2


treeView : String -> Int -> FileTree -> Html Msg
treeView teamName depth tree =
    let
        breadcrumbs path =
            Html.ul
                [ cssClass "breadcrumb" ]
                (path
                    |> String.split "/"
                    |> (\dirs ->
                            prefixes dirs
                                |> List.map (String.join "/")
                                |> zip dirs
                       )
                    |> List.map
                        (\( dir, prefix ) ->
                            Html.li
                                [ Html.Events.onClick <| Focus prefix
                                , folderClass
                                ]
                                [ text <|
                                    if dir == "" then
                                        teamName
                                    else
                                        dir
                                ]
                        )
                )
    in
    treeView_ (Just <| Html.h2 [] <| List.singleton <| breadcrumbs <| .path <| itemEntry tree) depth tree


treeView_ : Maybe (Html Msg) -> Int -> FileTree -> Html Msg
treeView_ title depth tree =
    let
        childViews children =
            Html.ul []
                (Dict.values children
                    |> List.sortBy (itemEntry >> .path >> String.toUpper)
                    |> List.map
                        (\t -> Html.li [] [ treeView_ Nothing (depth - 1) t ])
                )
    in
    case tree of
        File entry ->
            Html.div
                [ cssClass "file-item" ]
                [ text <| takeFileName <| entry.path
                , Html.span [ cssClass "size" ] [ text <| humanize <| Maybe.withDefault 0 entry.size ]
                ]

        Dir entry size children ->
            Html.div
                []
                [ Html.a
                    [ cssClass "folder file-item" ]
                    [ Maybe.withDefault (Html.a [ Html.Events.onClick <| Focus entry.key ] [ text <| takeFileName <| entry.path ]) title
                    , Html.span [ cssClass "size" ] [ text <| humanize size ]
                    ]
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


folderClass =
    cssClass "folder"


cssClass name =
    Html.Attributes.property "className" (Encode.string name)


ifDiv test html =
    if test then
        html
    else
        div [] []


isSignedIn model =
    model.accountInfo |> Maybe.map (always True) |> Maybe.withDefault False
