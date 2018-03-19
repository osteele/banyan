module Model exposing (..)

import Data exposing (..)
import Dropbox
import FileTree exposing (FileTree)
import FilesComponent
import Navigation


type alias FilesModel =
    FilesComponent.Model


type SignInStatus
    = SignedOut
    | SignedIn


type alias Model =
    { location : Navigation.Location
    , clientId : String
    , debug : Maybe String

    -- account
    , accountInfo : Maybe AccountInfo
    , auth : Maybe Dropbox.UserAuth
    , status : SignInStatus

    -- tree and loading status
    , files : FilesModel

    -- view state
    , path : String
    , depth : Int
    , order : SortOrder
    }


init : { a | clientId : String, files : Maybe String } -> Navigation.Location -> Model
init { clientId, files } location =
    { location = location
    , debug = Nothing

    -- account
    , accountInfo = Nothing
    , auth = Nothing
    , clientId = clientId
    , status = SignedOut

    -- tree and loading status
    , files = FilesComponent.fromCache files

    -- view state
    , path = "/"
    , depth = 1
    , order = Alphabetic
    }



-- ACCOUNT


clearAccountFields : Model -> Model
clearAccountFields model =
    { model
        | auth = Nothing
        , accountInfo = Nothing
        , status = SignedOut
        , files = FilesComponent.init
        , path = "/"
    }


signedIn : Model -> Bool
signedIn model =
    case model.status of
        SignedIn ->
            True

        _ ->
            False


signedOut : Model -> Bool
signedOut model =
    case model.status of
        SignedOut ->
            True

        _ ->
            False


teamName : Model -> String
teamName model =
    model.accountInfo |> Maybe.map .teamName |> Maybe.withDefault "Personal"



-- FILES


subtree : Model -> FileTree
subtree model =
    model.files.files
        |> FileTree.getSubtree model.path
        |> Maybe.withDefault model.files.files


subtreeTitle : Model -> String
subtreeTitle model =
    let
        path =
            subtree model
                |> FileTree.nodePath
    in
        teamName model ++ path
