port module Main exposing (..)

import Debug
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
        , view = view
        , onAuth = AuthResponse
        }



-- model


type alias Model =
    { auth : Maybe Dropbox.UserAuth
    , clientId : String
    , debug : Maybe String
    , location : Navigation.Location
    }


init : Navigation.Location -> Model
init location =
    { auth = Nothing
    , clientId = ""
    , debug = Nothing
    , location = location
    }



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

        AuthResponse (Dropbox.AuthorizeOk auth) ->
            ( { model | auth = Just auth.userAuth }, Cmd.none )

        AuthResponse _ ->
            ( { model | auth = Nothing }, Cmd.none )

        ListFiles ->
            ( model, model.auth |> Maybe.andThen extractAccessToken |> Maybe.withDefault "" |> listFiles )

        FileList s ->
            let
                _ =
                    Debug.log "fileList" s
            in
            ( model, Cmd.none )



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
        [ div []
            [ model.auth
                |> Maybe.andThen extractAccessToken
                |> Maybe.map redact
                |> Maybe.withDefault "not signed in"
                |> text
            ]
        , Html.button
            [ Html.Events.onClick SignIn ]
            [ Html.text "Sign into Dropbox" ]
        , Html.button
            [ Html.Events.onClick ListFiles ]
            [ Html.text "List files" ]
        , div [] [ model.debug |> Maybe.withDefault "" |> text ]
        ]


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


redact : String -> String
redact s =
    if String.length s > 6 then
        String.left 3 s ++ "…" ++ String.right 3 s
    else
        s
