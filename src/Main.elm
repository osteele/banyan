module Main exposing (..)

import Dropbox
import Dropbox.AccountInfo exposing (..)
import Cmd.Extras exposing (..)
import FilesComponent
import Message exposing (..)
import Model exposing (..)
import Ports exposing (..)
import Update exposing (..)
import View exposing (..)


type alias Flags =
    { accessToken : Maybe String
    , clientId : String
    , files : Maybe String
    }


main : Program Flags Model (Dropbox.Msg Msg)
main =
    Dropbox.programWithFlags
        { init =
            \flags location ->
                Model.init flags location
                    ! [ message <| InitializeAccessToken flags.accessToken ]
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onAuth = AuthResponse
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ setPath SetFocus
        , Sub.map FilesMessage <| FilesComponent.subscriptions model.files
        ]
