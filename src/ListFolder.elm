port module ListFolder exposing (..)

import Dropbox
import FileEntry exposing (..)
import Json.Encode
import Json.Decode exposing (..)


-- Elm -> JS


port saveFilesCache : Json.Encode.Value -> Cmd msg



-- JS -> ELM


port receiveFileList : (( Json.Encode.Value, Bool ) -> msg) -> Sub msg


decodeFileList : ( Value, Bool ) -> Result String ( List Dropbox.Metadata, Bool )
decodeFileList ( data, more ) =
    decodeValue (list decodeFileEntry) data
        |> Result.map (\entries -> ( entries, more ))


port receiveFileListError : (String -> msg) -> Sub msg
