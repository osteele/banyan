port module ListFolder exposing (..)

import Dropbox
import FileEntry exposing (..)
import Json.Encode
import Json.Decode exposing (..)


-- Elm -> JS


{-| listFolder parameters.

Doesn't implement include_media_info, include_has_explicit_shared_members, include_mounted_folders.

-}
type alias ListFolderParameters =
    { path : String
    , recursive : Bool
    , includeDeleted : Bool
    }


{-| Request that JavaScript start initiating requests to Dropbox.

See: // See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder>

-}
port listFolder : ( String, ListFolderParameters ) -> Cmd msg


port saveFilesCache : Json.Encode.Value -> Cmd msg



-- JS -> ELM


port receiveFileList : (( Json.Encode.Value, Bool ) -> msg) -> Sub msg


decodeFileList : ( Value, Bool ) -> Result String ( List Dropbox.Metadata, Bool )
decodeFileList ( data, more ) =
    decodeValue (list decodeFileEntry) data
        |> Result.map (\entries -> ( entries, more ))


port receiveFileListError : (String -> msg) -> Sub msg
