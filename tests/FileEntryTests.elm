module FileEntryTests exposing (..)

import Dict
import Expect exposing (Expectation)
import FileEntry exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "FileTree"
        [ test "/dir/ creates dir node" <|
            \_ ->
                fromFilePaths [ "/dir" ]
                    |> get "/dir"
                    |> Expect.equal (Just <| dirEntry "/dir")
        , test "/dir/leaf creates /dir node" <|
            \_ ->
                fromFilePaths [ "/dir/leaf" ]
                    |> get "/dir"
                    |> Expect.equal (Just <| dirEntry "/dir")
        , test "/dir/leaf creates leaf node" <|
            \_ ->
                fromFilePaths [ "/dir/leaf" ]
                    |> get "/dir/leaf"
                    |> Expect.equal (Just <| dirEntry "/dir/leaf")
        , test "/dir/leaf + /Dir preserves leaf node" <|
            \_ ->
                fromFilePaths [ "/dir/leaf", "/Dir" ]
                    |> get "/dir/leaf"
                    |> Expect.equal (Just <| dirEntry "/dir/leaf")
        , skip <|
            test "/dir/leaf + /Dir updates dir node" <|
                \_ ->
                    fromFilePaths [ "/dir/leaf", "/Dir" ]
                        |> get "/dir"
                        |> Expect.equal (Just <| dirEntry "/Dir")
        ]


fromFilePaths : List String -> FileEntry.FileTree
fromFilePaths paths =
    FileEntry.empty
        |> addFileEntries (List.map (\p -> FileEntry "dir" p p Nothing) paths)


nodeChildren : FileEntry.FileTree -> Dict.Dict String FileTree
nodeChildren tree =
    case tree of
        Dir _ _ children ->
            children

        _ ->
            Dict.empty


get : String -> FileTree -> Maybe FileEntry
get path tree =
    let
        f : List String -> FileTree -> Maybe FileEntry
        f p t =
            case p of
                [] ->
                    Just <| itemEntry t

                d :: [] ->
                    nodeChildren t |> Dict.get d |> Maybe.map itemEntry

                d :: pt ->
                    nodeChildren t |> Dict.get d |> Maybe.andThen (f pt)
    in
    f (dropPrefix "/" path |> Maybe.withDefault path |> String.split "/") tree


dirEntry : String -> FileEntry
dirEntry path =
    FileEntry "dir" path path Nothing
