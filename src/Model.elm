module Model
    exposing
        ( Model
        , SignInStatus(..)
        , SortOrder(..)
        , clearAccountFields
        , combineErrors
        , init
        , isSignedIn
        , isSignedOut
        , subtree
        , teamName
        )

import Dropbox
import Dropbox.AccountInfo exposing (..)
import Dropbox.FileTree as FileTree exposing (FileTree)
import FilesComponent
import Maybe.Extra as Maybe
import Navigation


type alias FilesModel =
    FilesComponent.Model


type SignInStatus
    = SignedOut
    | SigningIn
    | SignedIn


type SortOrder
    = Alphabetic
    | AscendingSize
    | DescendingSize


type alias Model =
    { location : Navigation.Location
    , clientId : String
    , errors : List String

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
    , errors = []

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


combineErrors : Model -> Model
combineErrors model =
    case model.files.error of
        Nothing ->
            model

        Just err ->
            let
                files =
                    model.files
            in
                { model
                    | errors = err :: model.errors
                    , files = { files | error = Nothing }
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


isSignedIn : Model -> Bool
isSignedIn model =
    case model.status of
        SignedIn ->
            True

        _ ->
            False


isSignedOut : Model -> Bool
isSignedOut model =
    case model.status of
        SignedOut ->
            True

        _ ->
            False


teamName : Model -> String
teamName model =
    model.accountInfo
        |> Maybe.map .team
        |> Maybe.join
        |> Maybe.map .name
        |> Maybe.withDefault "Personal"



-- FILES


{-| The currently-focused subtree.
-}
subtree : Model -> FileTree
subtree model =
    let
        tree =
            model.files.files
    in
        tree
            |> FileTree.getSubtree model.path
            |> Maybe.withDefault tree
