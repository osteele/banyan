module Utils exposing (..)

import Regex
import Round


dropPrefix : String -> String -> Maybe String
dropPrefix prefix s =
    if String.startsWith prefix s then
        s |> String.dropLeft (String.length prefix) |> Just
    else
        Nothing


firstMatch : Regex.Regex -> String -> Maybe String
firstMatch re s =
    case Regex.find Regex.All re s of
        { submatches } :: _ ->
            case submatches of
                [ s ] ->
                    s

                _ ->
                    Nothing

        _ ->
            Nothing


humanize : Int -> String
humanize n =
    case List.filter (\( s, _ ) -> toFloat n > s) [ ( 1.0e12, "T" ), ( 1.0e9, "G" ), ( 1.0e6, "M" ), ( 1.0e3, "K" ) ] of
        ( s, unit ) :: _ ->
            (toFloat n / s |> Round.round 1) ++ unit

        _ ->
            toString n ++ " bytes"


takeFileName : String -> String
takeFileName path =
    path |> String.split "/" |> List.foldl always path
