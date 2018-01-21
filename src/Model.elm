module Model exposing (..)

import Data exposing (..)
import Dropbox
import FileEntry exposing (FileEntry)
import FileTree exposing (..)
import Navigation


type alias Model =
    { location : Navigation.Location
    , auth : Maybe Dropbox.UserAuth
    , clientId : String
    , debug : Maybe String
    , fileTree : FileTree
    , loadingTree : Bool
    , loadedEntryCount : Int
    , requestCount : Int
    , accountInfo : Maybe AccountInfo
    , path : String
    , depth : Int
    }


init : String -> Navigation.Location -> Model
init clientId location =
    { location = location
    , auth = Nothing
    , clientId = clientId
    , debug = Nothing
    , fileTree = FileTree.empty
    , loadingTree = False
    , loadedEntryCount = 0
    , requestCount = 0
    , accountInfo = Nothing
    , path = "/"
    , depth = 1
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
    model.fileTree |> getSubtree model.path |> Maybe.withDefault model.fileTree


teamName : Model -> String
teamName model =
    model.accountInfo |> Maybe.map .teamName |> Maybe.withDefault "Personal"
