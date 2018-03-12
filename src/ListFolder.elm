port module ListFolder exposing (..)

import Dropbox exposing (UserAuth)
import FileEntry exposing (..)
import Json.Encode
import Json.Decode exposing (..)


-- Elm -> JS
-- from https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder


type alias ListFolderParameters =
    { path : String
    , recursive : Bool
    , includeDeleted : Bool
    }


{-| Request that JavaScript start initiating requests to Dropbox.
-}
port listFolder : ( String, ListFolderParameters ) -> Cmd msg


port saveFilesCache : String -> Cmd msg



-- JS -> ELM


port receiveFileList : (( Json.Encode.Value, Bool ) -> msg) -> Sub msg


decodeFileList : ( Value, Bool ) -> Result String ( List FileEntry, Bool )
decodeFileList ( data, more ) =
    decodeValue (list decodeFileEntry) data
        |> Result.map (\entries -> ( entries, more ))


port receiveFileListError : (String -> msg) -> Sub msg
