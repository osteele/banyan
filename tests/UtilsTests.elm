module UtilsTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Utils exposing (..)


suite : Test
suite =
    describe "Utils"
        [ describe "dropPrefix"
            [ test " drops" <|
                \_ ->
                    dropPrefix "/" "/a/b"
                        |> Expect.equal (Just "a/b")
            , test "dropPrefix drops doesn't drop" <|
                \_ ->
                    dropPrefix "/" "a/b"
                        |> Expect.equal Nothing
            , test "dropPrefix empty string" <|
                \_ ->
                    dropPrefix "/" ""
                        |> Expect.equal Nothing
            ]
        , describe "humanize"
            [ test "gigabytes" <|
                \_ ->
                    humanize 1234567890
                        |> Expect.equal "1.2G"
            , test "small sizes" <|
                \_ ->
                    humanize 123
                        |> Expect.equal "123 bytes"
            ]
        , describe "takeFileName"
            [ test "returns the final component" <|
                \_ ->
                    takeFileName "/a/b/base.ext"
                        |> Expect.equal "base.ext"
            , test "returns the only component" <|
                \_ ->
                    takeFileName "base.ext"
                        |> Expect.equal "base.ext"
            ]
        ]
