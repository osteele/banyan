module Model exposing (..)

import Data exposing (..)
import Dropbox
import FileEntry exposing (..)
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


init : Navigation.Location -> Model
init location =
    { location = location
    , auth = Nothing
    , clientId = ""
    , debug = Nothing
    , fileTree = FileEntry.empty
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
        , fileTree = FileEntry.empty
        , loadingTree = False
        , loadedEntryCount = 0
        , requestCount = 0
        , path = "/"
    }


isSignedIn : Model -> Bool
isSignedIn model =
    model.accountInfo |> Maybe.map (always True) |> Maybe.withDefault False
