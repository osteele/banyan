port module Ports exposing (..)

import Json.Encode


-- Elm -> JS


port storeAccessToken : Json.Encode.Value -> Cmd msg


port removeAccountInfo : () -> Cmd msg



-- JS -> ELM


port saveFilesCache : Json.Encode.Value -> Cmd msg


port setPath : (String -> msg) -> Sub msg
