port module Main exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Debug
import Regex
import Dropbox
import Navigation
import Config
import Time exposing (Time, second)

main : Program Never Model (Dropbox.Msg Msg)
main =
  Dropbox.program
    { init = \location -> ( init location, requestConfig () )
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onAuth = AuthResponse
    }

-- model

type alias Model =
  { auth: Maybe Dropbox.UserAuth
  , clientId: String
  , debug: Maybe String
  , location : Navigation.Location
  }

init : Navigation.Location -> Model
init location =
  { auth = Nothing
  , clientId = Config.dropboxClientID
  , debug = Nothing
  , location = location
   }

-- messages

type Msg
  = SignIn
  | ClientID String
  | AuthResponse Dropbox.AuthorizeResult
  | ListFiles
  | FileList String
  | Tick Time

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    ClientID clientId ->
      let _ = Debug.log "clientId =" clientId
      in ( { model | clientId = clientId }, Cmd.none)

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
      ({ model | auth = Just auth.userAuth }, Cmd.none)

    AuthResponse _ ->
      ({ model | auth = Nothing }, Cmd.none)
            -- |> update (DebugResult <| toString <| msg)

    ListFiles ->
      (model, model.auth |> Maybe.andThen extractAccessToken |> Maybe.withDefault "" |> listFiles)

    FileList s ->
      let _ = Debug.log "fileList" s
      in (model, Cmd.none)

    Tick t ->
      let _ = Debug.log "time" t
      in ({model| debug = Just (toString t)}, Cmd.none)

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
      dropboxClientID ClientID
    , fileList FileList
    , Time.every second Tick
    ]

port requestConfig : () -> Cmd msg
port dropboxClientID : (String -> msg) -> Sub msg

port listFiles : String -> Cmd msg
port fileList : (String -> msg) -> Sub msg

-- view

view : Model -> Html Msg
view model =
  div []
    [ div [] [ model.clientId |> redact |> text ]
    -- [ div [] [ model.clientId |> Maybe.map toString |> Maybe.withDefault "Loading…" |> text ]
    , div [] [
        model.auth
        |> Maybe.andThen extractAccessToken
        |> Maybe.map redact
        |> Maybe.withDefault "not signed in"
        |> text ]
    , Html.button
        [ Html.Events.onClick SignIn ]
        [ Html.text "Sign into Dropbox" ]
    , Html.button
        [ Html.Events.onClick ListFiles ]
        [ Html.text "List files" ]
    , div [] [ model.debug |> Maybe.withDefault "-" |> text]
    ]

extractAccessToken : Dropbox.UserAuth -> Maybe String
extractAccessToken auth =
  auth |> toString |> firstMatch (Regex.regex "Bearer \"(.+)\"")

firstMatch : Regex.Regex -> String -> Maybe String
firstMatch re s =
  case Regex.find Regex.All re s of
    { submatches } :: _ ->
      case submatches of
        [s] ->
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
