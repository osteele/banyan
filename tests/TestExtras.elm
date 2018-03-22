module TestExtras exposing (..)

import Json.Encode as Encode
import Expect exposing (Expectation)
import Test exposing (..)


expectJust : a -> Maybe a -> Expectation
expectJust =
    Expect.equal << Just


expectJsonEqual : Encode.Value -> Encode.Value -> Expectation
expectJsonEqual a =
    Expect.all
        [ Expect.equal a
        , flip Expect.equal a
        ]


pending : a -> String -> b -> Test
pending _ s _ =
    test s <|
        \_ ->
            let
                _ =
                    Debug.log "skipping test" s
            in
                Expect.pass
