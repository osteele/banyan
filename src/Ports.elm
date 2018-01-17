port module Ports exposing (..)

import Data exposing (..)
import FileEntry exposing (..)


port dropboxClientID : (String -> msg) -> Sub msg


port listFiles : String -> Cmd msg


port fileList : (( List FileEntry, Bool ) -> msg) -> Sub msg


port fileListError : (() -> msg) -> Sub msg


port getAccountInfo : String -> Cmd msg


port storeAccessToken : Maybe String -> Cmd msg


port setAccountInfo : (AccountInfo -> msg) -> Sub msg
