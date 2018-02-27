port module ListFolder exposing (..)

import Dropbox exposing (UserAuth)
import FileEntry exposing (..)


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



-- JS -> ELM


port receiveFileList : (( List FileEntry, Bool ) -> msg) -> Sub msg


port receiveFileListError : (Maybe String -> msg) -> Sub msg