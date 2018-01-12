port module Main exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Dropbox
import Navigation
import Config

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
  , location : Navigation.Location
  }

init : Navigation.Location -> Model
init location =
  { auth = Nothing
  , clientId = Config.dropboxClientID
  , location = location
   }

-- messages

type Msg
  = DropboxSignIn
  | ClientID String
  | AuthResponse Dropbox.AuthorizeResult

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    ClientID clientId ->
      ( { model | clientId = clientId }, Cmd.none)

    DropboxSignIn ->
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

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  dropboxClientID ClientID

port requestConfig : () -> Cmd msg

port dropboxClientID : (String -> msg) -> Sub msg

-- view

view : Model -> Html Msg
view model =
  div []
    -- [ div [] [ model.clientId |> toString |> text ]
    -- [ div [] [ model.clientId |> Maybe.map toString |> Maybe.withDefault "Loading…" |> text ]
    [ div [] [ model.auth |> Maybe.map toString |> Maybe.withDefault "not signed in" |> text ]
    , Html.button
        [ Html.Events.onClick DropboxSignIn ]
        [ Html.text "Sign into Dropbox" ]
    ]
