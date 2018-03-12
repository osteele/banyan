module Model exposing (..)

import Data exposing (..)
import Dropbox
import FileEntry
import FileTree exposing (FileTree)
import FilesModel exposing (FilesModel)
import Navigation


type alias Model =
    { location : Navigation.Location
    , clientId : String
    , debug : Maybe String

    -- account
    , accountInfo : Maybe AccountInfo
    , auth : Maybe Dropbox.UserAuth

    -- tree and loading status
    , files : FilesModel

    -- view state
    , path : String
    , depth : Int
    , order : SortOrder
    }


init : String -> Navigation.Location -> Model
init clientId location =
    { location = location
    , debug = Nothing

    -- account
    , accountInfo = Nothing
    , auth = Nothing
    , clientId = clientId

    -- tree and loading status
    , files = FilesModel.init

    -- view state
    , path = "/"
    , depth = 1
    , order = Alphabetic
    }



-- account


clearAccountFields : Model -> Model
clearAccountFields model =
    { model
        | auth = Nothing
        , accountInfo = Nothing
        , files = FilesModel.init
        , path = "/"
    }


isSignedIn : Model -> Bool
isSignedIn model =
    model.accountInfo |> Maybe.map (always True) |> Maybe.withDefault False


teamName : Model -> String
teamName model =
    model.accountInfo |> Maybe.map .teamName |> Maybe.withDefault "Personal"



-- files


subtree : Model -> FileTree
subtree model =
    model.files.fileTree
        |> FileTree.getSubtree model.path
        |> Maybe.withDefault model.files.fileTree


subtreeTitle : Model -> String
subtreeTitle model =
    let
        path =
            subtree model
                |> FileTree.itemEntry
                |> FileEntry.path
    in
        teamName model ++ path
