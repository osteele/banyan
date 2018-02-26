port module Ports exposing (..)

import Data exposing (..)


-- Elm -> JS


port getAccountInfo : String -> Cmd msg


port storeAccessToken : String -> Cmd msg


port signOut : () -> Cmd msg



-- JS -> ELM


port receiveAccountInfo : (AccountInfo -> msg) -> Sub msg


port setPath : (String -> msg) -> Sub msg
