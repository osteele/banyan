port module Ports exposing (..)

import Data exposing (..)
import FileEntry exposing (..)


-- Elm -> JS


port storeAccessToken : String -> Cmd msg


port getAccountInfo : String -> Cmd msg


port signOut : () -> Cmd msg


port listFiles : ( String, Bool ) -> Cmd msg



-- JS -> ELM


port receiveFileList : (( List FileEntry, Bool ) -> msg) -> Sub msg


port receiveFileListError : (Maybe String -> msg) -> Sub msg


port receiveAccountInfo : (AccountInfo -> msg) -> Sub msg


port setPath : (String -> msg) -> Sub msg
