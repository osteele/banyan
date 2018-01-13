port module Main exposing (..)

import BeautifulExample
import Color
import Debug
import Dict
import Dropbox
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Navigation
import Regex


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
    , tree : SizeTree
    }


init : Navigation.Location -> Model
init location =
    { location = location
    , auth = Nothing
    , clientId = ""
    , debug = Nothing
    , tree = emptySizeTree "/"
    }


type SizeTree
    = Dir String Int (Dict.Dict String SizeTree)
    | File String Int


emptySizeTree : String -> SizeTree
emptySizeTree name =
    Dir name 0 Dict.empty


itemName : SizeTree -> String
itemName item =
    case item of
        Dir n _ _ ->
            n

        File n _ ->
            n


itemSize : SizeTree -> Int
itemSize item =
    case item of
        Dir _ s _ ->
            s

        File _ s ->
            s


updateSize : SizeTree -> SizeTree
updateSize tree =
    case tree of
        Dir n _ children ->
            let
                s =
                    children |> Dict.values |> List.map itemSize |> List.sum
            in
            Dir n s children

        file ->
            file


addItem : List String -> Int -> SizeTree -> SizeTree
addItem filePath fileSize tree =
    case filePath of
        name :: [] ->
            case tree of
                Dir n s children ->
                    let
                        ch =
                            Dict.insert name (File name fileSize) children
                    in
                    Dir n s ch |> updateSize

                _ ->
                    File name fileSize

        dir :: subdir ->
            let
                emptyDirTree =
                    emptySizeTree dir

                update : Maybe SizeTree -> SizeTree
                update maybeTree =
                    addItem subdir fileSize (Maybe.withDefault emptyDirTree maybeTree)
            in
            case tree of
                Dir n s children ->
                    Dir n s (children |> Dict.update dir (Just << update)) |> updateSize

                _ ->
                    addItem subdir fileSize emptyDirTree

        [] ->
            tree


updateTree : List FileEntry -> SizeTree -> SizeTree
updateTree files tree =
    let
        components filePath =
            filePath |> dropPrefix "/" |> Maybe.withDefault filePath |> String.split "/"

        update item tree =
            addItem (components item.path) item.size tree
    in
    List.foldl update tree <| List.filter (\f -> f.tag == "file") files



-- messages


type Msg
    = SignIn
    | ClientID String
    | AuthResponse Dropbox.AuthorizeResult
    | ListFiles
    | FileList (List FileEntry)


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

        FileList files ->
            ( { model | tree = updateTree files model.tree }, Cmd.none )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dropboxClientID ClientID
        , fileList FileList
        ]


port dropboxClientID : (String -> msg) -> Sub msg


type alias FileEntry =
    { tag : String
    , path : String
    , size : Int
    }


port listFiles : String -> Cmd msg


port fileList : (List FileEntry -> msg) -> Sub msg



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
        , div [] [ model.debug |> Maybe.withDefault "" |> text ]
        , treeView_ 1 model.tree
        ]


treeView_ depth tree =
    -- let
    --     _ =
    --         Debug.log "tree" tree
    -- in
    treeView depth tree


treeView depth tree =
    let
        fmt name size =
            name ++ " (" ++ humanize size ++ ")"

        childViews children =
            Html.ul []
                (Dict.values children
                    |> List.sortBy itemName
                    |> List.map
                        (\t -> Html.li [] [ treeView (depth - 1) t ])
                )
    in
    case tree of
        File name size ->
            div [] [ text <| fmt name size ]

        Dir name size children ->
            div []
                [ text <| fmt name size
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
            toString (toFloat n / s) ++ unit

        _ ->
            toString n ++ " bytes"


dropPrefix : String -> String -> Maybe String
dropPrefix prefix s =
    if String.startsWith prefix s then
        s |> String.dropLeft (String.length prefix) |> Just
    else
        Nothing
