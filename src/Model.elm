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
    , files : FileSyncModel

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
    , files = initFileSyncModel

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
        , files = initFileSyncModel
        , path = "/"
    }


isSignedIn : Model -> Bool
isSignedIn model =
    model.accountInfo |> Maybe.map (always True) |> Maybe.withDefault False


teamName : Model -> String
teamName model =
    model.accountInfo |> Maybe.map .teamName |> Maybe.withDefault "Personal"



-- files


type alias FileSyncModel =
    { fileTree : FileTree
    , syncing : Bool
    , syncedEntryCount : Int
    , requestCount : Int
    , errorMessage : Maybe String
    }


initFileSyncModel : FileSyncModel
initFileSyncModel =
    { fileTree = FileTree.empty
    , syncing = False
    , syncedEntryCount = 0
    , requestCount = 0
    , errorMessage = Nothing
    }


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
                |> .path
    in
    teamName model ++ path
