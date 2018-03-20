port module Ports exposing (..)

-- Elm -> JS


port storeAccessToken : String -> Cmd msg


port removeAccountInfo : () -> Cmd msg



-- JS -> ELM


port setPath : (String -> msg) -> Sub msg
