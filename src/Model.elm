module Model exposing (..)

import Data exposing (..)
import Dropbox
import FileTree exposing (FileTree)
import Navigation


type alias Model =
    { location : Navigation.Location
    , clientId : String
    , debug : Maybe String

    -- account
    , accountInfo : Maybe AccountInfo
    , auth : Maybe Dropbox.UserAuth

    -- tree and loading status
    , fileTree : FileTree
    , loadingTree : Bool
    , loadedEntryCount : Int
    , requestCount : Int

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
    , fileTree = FileTree.empty
    , loadingTree = False
    , loadedEntryCount = 0
    , requestCount = 0

    -- view state
    , path = "/"
    , depth = 1
    , order = Alphabetic
    }


clearAccountFields : Model -> Model
clearAccountFields model =
    { model
        | auth = Nothing
        , accountInfo = Nothing
        , fileTree = FileTree.empty
        , loadingTree = False
        , loadedEntryCount = 0
        , requestCount = 0
        , path = "/"
    }


isSignedIn : Model -> Bool
isSignedIn model =
    model.accountInfo |> Maybe.map (always True) |> Maybe.withDefault False


subtree : Model -> FileTree
subtree model =
    model.fileTree
        |> FileTree.getSubtree model.path
        |> Maybe.withDefault model.fileTree


subtreeTitle : Model -> String
subtreeTitle model =
    let
        path =
            subtree model
                |> FileTree.itemEntry
                |> .path
    in
    teamName model ++ path


teamName : Model -> String
teamName model =
    model.accountInfo |> Maybe.map .teamName |> Maybe.withDefault "Personal"
