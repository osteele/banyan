port module Ports exposing (..)

import Data exposing (..)
import FileEntry exposing (..)


port listFiles : String -> Cmd msg


port receiveFileList : (( List FileEntry, Bool ) -> msg) -> Sub msg


port receiveFileListError : (() -> msg) -> Sub msg


port getAccountInfo : String -> Cmd msg


port receiveAccountInfo : (AccountInfo -> msg) -> Sub msg


port storeAccessToken : Maybe String -> Cmd msg


port signOut : () -> Cmd msg


port setPath : (String -> msg) -> Sub msg
