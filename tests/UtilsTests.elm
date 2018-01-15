module UtilsTests exposing (..)

import Expect exposing (Expectation)
import Regex
import Test exposing (..)
import Tuple exposing (mapFirst)
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
        , test "firstMatch" <|
            \_ ->
                [ ( "(\\d)", "a1b" )
                , ( "(\\d)", "a1b2c" )
                , ( "(\\d)", "abc" )
                ]
                    |> List.map (mapFirst Regex.regex)
                    |> List.map (uncurry firstMatch)
                    |> Expect.equal
                        [ Just "1"
                        , Just "1"
                        , Nothing
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
        , test "prefixes" <|
            \_ ->
                prefixes [ "a", "b", "c" ]
                    |> Expect.equal
                        [ [ "a" ]
                        , [ "a", "b" ]
                        , [ "a", "b", "c" ]
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
