module Extras
    exposing
        ( dropPrefix
        , firstMatch
        , flatMapS
        , humanize
        , ifJust
        , mapS
        , mapValues
        , maybeToDefault
        , pluralize
        , prefixes
        , quantify
        , remove
        , takeFileName
        , toStringWithCommas
        , zip
        )

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.


# Dict

@docs mapValues


# Inflect

@docs humanize, pluralize, quantify, toStringWithCommas


# List

@docs flatMapM, mapS, remove, zip


# Maybe

@docs ifJust, maybeToDefault


# Paths

@docs takeFileName


# Regex

@docs firstMatch


# Strings

@docs dropPrefix, prefixes

-}

import Dict
import Regex
import Round
import String


-- DICT


mapValues : (a -> b) -> Dict.Dict comparable a -> Dict.Dict comparable b
mapValues f d =
    Dict.toList d
        |> List.map (Tuple.mapSecond f)
        |> Dict.fromList



-- INFLECT


{-| Turn a byte count into a string with units.

    humanize 1000000 == "1GB"
    humanize 1000000 == "1GB"

-}
humanize : Int -> String
humanize n =
    case List.filter (\( s, _ ) -> toFloat n > s) byteUnits of
        ( s, unit ) :: _ ->
            (toFloat n / s |> Round.round 1) ++ unit

        _ ->
            toString n ++ "B"


byteUnits : List ( Float, String )
byteUnits =
    [ ( 1.0e24, "YB" )
    , ( 1.0e21, "ZB" )
    , ( 1.0e18, "EB" )
    , ( 1.0e15, "PB" )
    , ( 1.0e12, "TB" )
    , ( 1.0e9, "GB" )
    , ( 1.0e6, "MB" )
    , ( 1.0e3, "kB" )
    ]


sibilantEnding : Regex.Regex
sibilantEnding =
    Regex.regex "(s|sh|ch)$"


pluralizesWithIes : Regex.Regex
pluralizesWithIes =
    Regex.regex "[^aeio]y$"


pluralize : String -> String
pluralize s =
    if Regex.contains sibilantEnding s then
        s ++ "es"
    else if String.endsWith "o" s then
        s ++ "es"
    else if Regex.contains pluralizesWithIes s then
        String.dropRight 1 s |> flip (++) "ies"
    else
        s ++ "s"


quantify : String -> number -> String
quantify s n =
    String.join " "
        [ toStringWithCommas n
        , if n == 1 then
            s
          else
            pluralize s
        ]


{-| Insert commas into a sequence of digits.

    toStringWithCommas "123" == "123"
    toStringWithCommas "1234" == "1,234"
    toStringWithCommas "$1234.5" == "$1,234.5"

-}
toStringWithCommas : num -> String
toStringWithCommas =
    toString
        >> String.reverse
        >> Regex.find Regex.All threeDigitsRegex
        >> List.map .match
        >> String.join ","
        >> String.reverse


threeDigitsRegex : Regex.Regex
threeDigitsRegex =
    Regex.regex "(\\d*\\.)?\\d{0,3}-?"



-- LIST


{-| Monadic flatMap.
-}
flatMapS : (s -> a -> ( List b, s )) -> s -> List a -> ( List b, s )
flatMapS f s xs =
    case xs of
        [] ->
            ( [], s )

        h :: t ->
            let
                ( r1, s1 ) =
                    f s h

                ( r2, s2 ) =
                    flatMapS f s1 t
            in
                ( r1 ++ r2, s2 )


{-| Remove the nth element from a list.
-}
remove : Int -> List a -> List a
remove n lst =
    List.take n lst ++ List.drop (n + 1) lst


{-| Monadic map.
-}
mapS : (s -> a -> ( b, s )) -> s -> List a -> ( List b, s )
mapS f s list =
    case list of
        [] ->
            ( [], s )

        x :: xs ->
            let
                ( y, s2 ) =
                    f s x

                ( ys, s3 ) =
                    mapS f s2 xs
            in
                ( y :: ys, s3 )


{-| Get a list of tuples from a tuple of lists.

    zip [1, 2, 3] ["one", "two", "three"] == [(1, "one"), (2, "two"), (3, "three")]

-}
zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)



-- MAYBE


ifJust : Bool -> a -> Maybe a
ifJust flag a =
    if flag then
        Just a
    else
        Nothing


maybeToDefault : a -> a -> Maybe a
maybeToDefault d a =
    if a == d then
        Nothing
    else
        Just a



-- PATH


{-| Get the POSIX filename.

    takeFileName "/dir/file.ext" == "file.ext"
    takeFileName "/dir/" == "dir"
    takeFileName "dir/" == "dir"

-}
takeFileName : String -> String
takeFileName path =
    path |> String.split "/" |> List.foldl always path



-- REGEX


{-| Find the first matching substring.

    firstMatch (Regex) == Just "sled"

-}
firstMatch : Regex.Regex -> String -> Maybe String
firstMatch re s =
    Regex.find (Regex.AtMost 1) re s
        |> List.head
        |> Maybe.map .submatches
        |> Maybe.andThen (List.head >> Maybe.withDefault Nothing)



-- STRING


{-| Remove the prefix, if present.

    dropPrefix "bob" "bobsled" == Just "sled"
    dropPrefix "rob" "bobsled" == Nothing
    (dropPrefix "rob" "bobsled" |> Maybe.withDefault "bobsled") == "bobsled"

-}
dropPrefix : String -> String -> Maybe String
dropPrefix prefix s =
    if String.startsWith prefix s then
        s |> String.dropLeft (String.length prefix) |> Just
    else
        Nothing


{-| Get the non-null prefixes of a list.

    prefixes "abc" == ["a", "ab", "abc"]

-}
prefixes : List a -> List (List a)
prefixes xs =
    case xs of
        h :: t ->
            [ h ] :: List.map ((::) h) (prefixes t)

        [] ->
            []
