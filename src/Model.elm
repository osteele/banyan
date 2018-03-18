module Model exposing (..)

import Data exposing (..)
import Dropbox
import FileTree exposing (FileTree)
import FilesComponent
import Navigation


type alias Model =
    { location : Navigation.Location
    , clientId : String
    , debug : Maybe String

    -- account
    , accountInfo : Maybe AccountInfo
    , auth : Maybe Dropbox.UserAuth

    -- tree and loading status
    , files : FilesComponent

    -- view state
    , path : String
    , depth : Int
    , order : SortOrder
    }


type alias FilesComponent =
    FilesComponent.Model


type alias Flags =
    { accessToken : Maybe String
    , clientId : String
    , files : Maybe String
    }


init : Flags -> Navigation.Location -> Model
init { clientId, files } location =
    { location = location
    , debug = Nothing

    -- account
    , accountInfo = Nothing
    , auth = Nothing
    , clientId = clientId

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
        , files = FilesComponent.init
        , path = "/"
    }


isSignedIn : Model -> Bool
isSignedIn model =
    model.accountInfo |> Maybe.map (always True) |> Maybe.withDefault False


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
