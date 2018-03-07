module FileTreeTests exposing (..)

import Dict
import Expect exposing (Expectation)
import FileEntry exposing (..)
import FileTree exposing (..)
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
        , test "delete /dir deletes directory" <|
            \_ ->
                [ dirEntry "/dir"
                , FileEntry deleteTag "/dir" "/dir" Nothing
                ]
                    |> fromEntries
                    |> get "/dir"
                    |> Expect.equal Nothing
        , test "delete /dir deletes subdirectory" <|
            \_ ->
                [ dirEntry "/dir"
                , dirEntry "/dir/subdir"
                , FileEntry deleteTag "/dir" "/dir" Nothing
                ]
                    |> fromEntries
                    |> get "/dir/subdir"
                    |> Expect.equal Nothing
        , test "delete /dir/subdir deletes subdirectory" <|
            \_ ->
                [ dirEntry "/dir"
                , dirEntry "/dir/subdir"
                , FileEntry deleteTag "/dir/subdir" "/dir/subdir" Nothing
                ]
                    |> fromEntries
                    |> Expect.all
                        [ get "/dir" >> Expect.equal (Just <| dirEntry "/dir")
                        , get "/dir/subdir" >> Expect.equal Nothing
                        ]
        , test "trimDepth" <|
            \_ ->
                fromFilePathS "/a;/a/b"
                    |> trimDepth 1
                    |> Expect.all
                        [ get "/a" >> Expect.equal (Just <| dirEntry "/a")
                        , get "/a/b" >> Expect.equal Nothing
                        ]
        ]


fromEntries : List FileEntry -> FileTree
fromEntries entries =
    FileTree.empty |> addEntries entries


fromFilePaths : List String -> FileTree
fromFilePaths paths =
    paths
        |> List.map (\p -> FileEntry dirTag (String.toLower p) p Nothing)
        |> fromEntries


fromFilePathS : String -> FileTree
fromFilePathS =
    String.split ";" >> fromFilePaths


dirEntry : String -> FileEntry
dirEntry path =
    FileEntry "folder" (String.toLower path) path Nothing
