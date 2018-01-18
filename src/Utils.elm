module Utils exposing (..)

import Dict
import Regex
import Round
import String


{-| Insert commas into a sequence of digits.

    toStringWithCommas "123" == "123"
    toStringWithCommas "1234" == "1,234"
    toStringWithCommas "$1234.5" == "$1,234.5"

-}
toStringWithCommas : num -> String
toStringWithCommas =
    toString
        >> String.reverse
        >> Regex.find Regex.All (Regex.regex "(\\d*\\.)?\\d{0,3}-?")
        >> List.map .match
        >> String.join ","
        >> String.reverse


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


flatMapM : (s -> a -> ( List b, s )) -> s -> List a -> ( List b, s )
flatMapM f s xs =
    case xs of
        [] ->
            ( [], s )

        h :: t ->
            let
                ( r1, s2 ) =
                    f s h

                ( r2, s3 ) =
                    flatMapM f s2 t
            in
            ( r1 ++ r2, s3 )


{-| Find the first matching substring.

    firstMatch (Regex) == Just "sled"

-}
firstMatch : Regex.Regex -> String -> Maybe String
firstMatch re s =
    Regex.find Regex.All re s
        |> List.head
        |> Maybe.map .submatches
        |> Maybe.andThen (List.head >> Maybe.withDefault Nothing)


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


ifJust : Bool -> a -> Maybe a
ifJust flag a =
    if flag then
        Just a
    else
        Nothing


mapValues : (a -> b) -> Dict.Dict comparable a -> Dict.Dict comparable b
mapValues f d =
    Dict.toList d
        |> List.map (Tuple.mapSecond f)
        |> Dict.fromList


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


{-| Get the POSIX filename.

    takeFileName "/directory/file.ext" == "file.ext"
    takeFileName "test/" == ""

-}
takeFileName : String -> String
takeFileName path =
    path |> String.split "/" |> List.foldl always path


{-| Get a list of tuples from a tuple of lists.

    zip [1, 2, 3] ["one", "two", "three"] == [(1, "one"), (2, "two"), (3, "three")]

-}
zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)
