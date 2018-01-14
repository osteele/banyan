port module Main exposing (..)

import BeautifulExample
import Color
import Dict
import Dropbox
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
    , tree : FileTree
    , loadingTree : Bool
    , loadedEntryCount : Int
    }


init : Navigation.Location -> Model
init location =
    { location = location
    , auth = Nothing
    , clientId = ""
    , debug = Nothing
    , tree = emptyFileTree "/"
    , loadingTree = False
    , loadedEntryCount = 0
    }


type FileTree
    = Dir FileEntry Int (Dict.Dict String FileTree)
    | File FileEntry


emptyFileTree : String -> FileTree
emptyFileTree name =
    Dir (FileEntry "dir" name name Nothing) 0 Dict.empty


itemEntry : FileTree -> FileEntry
itemEntry item =
    case item of
        Dir e _ _ ->
            e

        File e ->
            e


itemSize : FileTree -> Int
itemSize item =
    case item of
        Dir _ n _ ->
            n

        File e ->
            e.size |> Maybe.withDefault 0


recalcSize : FileTree -> FileTree
recalcSize tree =
    case tree of
        Dir e _ children ->
            let
                s =
                    children |> Dict.values |> List.map itemSize |> List.sum
            in
            Dir e s children

        file ->
            file


updateTreeItem : List String -> (Maybe FileTree -> FileTree) -> List String -> FileTree -> FileTree
updateTreeItem ks alter path tree =
    let
        hereNode =
            let
                name =
                    String.join "/" path
            in
            FileEntry "dir" name name Nothing

        subNode next =
            let
                name =
                    String.join "/" (path ++ [ next ])
            in
            FileEntry "dir" name name Nothing

        withDirItem fn =
            case tree of
                Dir entry size children ->
                    fn entry size children

                _ ->
                    fn hereNode 0 Dict.empty

        df k =
            Maybe.withDefault (Dir (subNode k) 0 Dict.empty)
    in
    case ks of
        [] ->
            alter <| Just tree

        k :: [] ->
            withDirItem <|
                \entry _ children ->
                    Dict.update k (Just << alter) children |> Dir entry 0 |> recalcSize

        k :: ks ->
            withDirItem <|
                \entry _ children ->
                    Dict.update k (Just << (\t -> updateTreeItem ks alter (path ++ [ k ]) (df k t))) children
                        |> Dir entry 0
                        |> recalcSize


insertFileEntry : List String -> FileEntry -> FileTree -> FileTree
insertFileEntry ks entry =
    let
        updateFile _ =
            File entry

        updateDir dir =
            case dir of
                Just (Dir e size children) ->
                    Dir entry size children

                _ ->
                    Dir entry 0 Dict.empty
    in
    updateTreeItem ks
        (if entry.tag == "dir" then
            updateDir
         else
            updateFile
        )
        []


addFileEntries : List FileEntry -> FileTree -> FileTree
addFileEntries entries tree =
    let
        components path =
            path |> dropPrefix "/" |> Maybe.withDefault path |> String.split "/"

        addEntry entry =
            insertFileEntry (components entry.key) entry
    in
    List.foldl addEntry tree entries



-- messages


type Msg
    = SignIn
    | ClientID String
    | AuthResponse Dropbox.AuthorizeResult
    | ListFiles
    | FileList (List FileEntry) Bool


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        AuthResponse (Dropbox.AuthorizeOk auth) ->
            ( { model | auth = Just auth.userAuth }, Cmd.none )

        AuthResponse _ ->
            ( { model | auth = Nothing }, Cmd.none )

        ClientID clientId ->
            ( { model | clientId = clientId }, Cmd.none )

        SignIn ->
            ( model
            , Dropbox.authorize
                { clientId = model.clientId
                , state = Nothing
                , requireRole = Nothing
                , forceReapprove = False
                , disableSignup = False
                , locale = Nothing
                , forceReauthentication = False
                }
                model.location
            )

        ListFiles ->
            let
                cmd =
                    case model.auth |> Maybe.andThen extractAccessToken of
                        Just token ->
                            listFiles token

                        Nothing ->
                            Cmd.none
            in
            ( model, cmd )

        FileList entries loading ->
            ( { model
                | tree = addFileEntries entries model.tree
                , loadingTree = loading
                , loadedEntryCount = model.loadedEntryCount + List.length entries
              }
            , Cmd.none
            )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dropboxClientID ClientID
        , fileList <| uncurry FileList
        ]


port dropboxClientID : (String -> msg) -> Sub msg


type alias FileEntry =
    { tag : String
    , key : String
    , path : String
    , size : Maybe Int
    }


port listFiles : String -> Cmd msg


port fileList : (( List FileEntry, Bool ) -> msg) -> Sub msg



-- view


view : Model -> Html Msg
view model =
    div []
        [ Html.button
            [ Html.Events.onClick SignIn ]
            [ Html.text (model.auth |> Maybe.map (\_ -> "Sign out") |> Maybe.withDefault "Sign into Dropbox") ]
        , Html.button
            [ Html.Events.onClick ListFiles ]
            [ Html.text "List files" ]
        , div []
            [ text
                (if model.loadingTree then
                    "Loading… (" ++ toString model.loadedEntryCount ++ " entries loaded)"
                 else
                    toString model.loadedEntryCount ++ " entries loaded"
                )
            ]
        , div [] [ model.debug |> Maybe.withDefault "" |> text ]
        , treeView_ 2 model.tree
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


dropPrefix : String -> String -> Maybe String
dropPrefix prefix s =
    if String.startsWith prefix s then
        s |> String.dropLeft (String.length prefix) |> Just
    else
        Nothing


fileBase : String -> String
fileBase path =
    -- path |> String.split "/" |> List.foldl (Just |> always) Nothing |> Maybe.withDefault "a"
    path |> String.split "/" |> List.foldl (\a b -> b) path
