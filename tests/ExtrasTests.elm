module ExtrasTests exposing (..)

import Expect exposing (Expectation)
import Extras exposing (..)
import Regex
import Test exposing (..)
import Tuple exposing (mapFirst)


suite : Test
suite =
    describe "Extras" <|
        listTests
            ++ maybeTests
            ++ stringTests
            ++ regexTests
            ++ inflectionTests
            ++ [ describe "takeFileName"
                    [ test "returns the final component" <|
                        \_ ->
                            takeFileName "/a/b/base.ext"
                                |> Expect.equal "base.ext"
                    , test "returns the only component" <|
                        \_ ->
                            takeFileName "base.ext"
                                |> Expect.equal "base.ext"
                    , test "matches the directory name" <|
                        \_ ->
                            takeFileName "test/"
                                |> Expect.equal ""
                    ]
               ]


listTests : List Test
listTests =
    [ test "remove" <|
        Expect.all <|
            [ \_ -> remove 0 [ "a", "b" ] |> Expect.equal [ "b" ]
            , \_ -> remove 1 [ "a", "b", "c" ] |> Expect.equal [ "a", "c" ]
            , \_ -> remove 2 [ "a", "b" ] |> Expect.equal [ "a", "b" ]
            ]
    ]


maybeTests : List Test
maybeTests =
    [ test "maybeToDefault" <|
        Expect.all
            [ \_ ->
                maybeToDefault 0 0
                    |> Expect.equal Nothing
            , \_ ->
                maybeToDefault 0 1
                    |> Expect.equal (Just 1)
            ]
    ]


stringTests : List Test
stringTests =
    [ describe "dropPrefix"
        [ test "drops the prefix" <|
            \_ ->
                dropPrefix "/" "/a/b"
                    |> Expect.equal (Just "a/b")
        , test "dropPrefix doesn't drop non-prefices" <|
            \_ ->
                dropPrefix "/" "a/b"
                    |> Expect.equal Nothing
        , test "doesn't drop a prefix from the empty string" <|
            \_ ->
                dropPrefix "/" ""
                    |> Expect.equal Nothing
        ]
    , test "prefixes" <|
        \_ ->
            prefixes [ "a", "b", "c" ]
                |> Expect.equal
                    [ [ "a" ]
                    , [ "a", "b" ]
                    , [ "a", "b", "c" ]
                    ]
    ]


regexTests : List Test
regexTests =
    [ test "firstMatch" <|
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
    ]


inflectionTests : List Test
inflectionTests =
    [ describe "humanize"
        [ test "gigabytes" <|
            \_ ->
                humanize 1234567890
                    |> Expect.equal "1.2GB"
        , test "small sizes" <|
            \_ ->
                humanize 123
                    |> Expect.equal "123B"
        ]
    , describe "pluralize"
        -- from https://en.wikipedia.org/wiki/English_plurals#Regular_plurals
        [ test "sibilants" <|
            \_ ->
                List.map pluralize [ "kiss", "dish", "witch" ]
                    |> Expect.equal [ "kisses", "dishes", "witches" ]
        , test "sibilants ending in -e" <|
            \_ ->
                List.map pluralize [ "phase", "massage", "judge" ]
                    |> Expect.equal [ "phases", "massages", "judges" ]
        , test "voiceless consonants" <|
            \_ ->
                List.map pluralize [ "lap", "cat", "clock", "cuff", "death" ]
                    |> Expect.equal [ "laps", "cats", "clocks", "cuffs", "deaths" ]
        , test "nouns in -o" <|
            \_ ->
                List.map pluralize [ "hero", "potato", "volcano" ]
                    |> Expect.equal [ "heroes", "potatoes", "volcanoes" ]
        , test "nouns in -Cy" <|
            \_ ->
                List.map pluralize [ "cherry", "lady", "sky" ]
                    |> Expect.equal [ "cherries", "ladies", "skies" ]
        , test "nouns in -Vy" <|
            \_ ->
                List.map pluralize [ "cherry", "lady", "sky", "soliloquy", "day", "monkey" ]
                    |> Expect.equal [ "cherries", "ladies", "skies", "soliloquies", "days", "monkeys" ]
        , test "everything else" <|
            \_ ->
                List.map pluralize [ "boy", "girl", "chair" ]
                    |> Expect.equal [ "boys", "girls", "chairs" ]
        ]
    , describe "quantify"
        [ test "0" <|
            \_ ->
                quantify "request" 0
                    |> Expect.equal "0 requests"
        , test "1" <|
            \_ ->
                quantify "request" 1
                    |> Expect.equal "1 request"
        , test "2" <|
            \_ ->
                quantify "request" 2
                    |> Expect.equal "2 requests"
        ]
    ]
