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
        , test "/dir/leaf + /Dir updates dir node" <|
            \_ ->
                fromFilePaths [ "/dir/leaf", "/Dir" ]
                    |> get "/dir"
                    |> Expect.equal (Just <| dirEntry "/Dir")
        , test "/Dir + /dir/leaf + leaves dir node" <|
            \_ ->
                fromFilePaths [ "/Dir", "/dir/leaf" ]
                    |> get "/dir"
                    |> Expect.equal (Just <| dirEntry "/Dir")
        ]


fromFilePaths : List String -> FileEntry.FileTree
fromFilePaths paths =
    FileEntry.empty
        |> addEntries (List.map (\p -> FileEntry dirTag (String.toLower p) p Nothing) paths)


dirEntry : String -> FileEntry
dirEntry path =
    FileEntry "folder" (String.toLower path) path Nothing
