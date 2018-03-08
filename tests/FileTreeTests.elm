module FileTreeTests exposing (..)

import Expect exposing (Expectation)
import FileEntry exposing (..)
import FileTree exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "FileTree"
        [ test "fromString >> toString" <|
            \_ ->
                FileTree.fromString "/a;/b:;/c:12"
                    |> FileTree.toString
                    |> Expect.equal "/a;/b:;/c:12"
        , test "/dir/ creates dir node" <|
            \_ ->
                FileTree.fromString "/dir"
                    |> get "/dir"
                    |> Expect.equal (Just <| dirEntry "/dir")
        , test "/dir/leaf creates /dir node" <|
            \_ ->
                FileTree.fromString "/dir/leaf"
                    |> get "/dir"
                    |> Expect.equal (Just <| dirEntry "/dir")
        , test "/dir/leaf creates leaf node" <|
            \_ ->
                FileTree.fromString "/dir/leaf"
                    |> get "/dir/leaf"
                    |> Expect.equal (Just <| dirEntry "/dir/leaf")
        , test "/dir/leaf + /Dir preserves leaf node" <|
            \_ ->
                FileTree.fromString "/dir/leaf;/Dir"
                    |> get "/dir/leaf"
                    |> Expect.equal (Just <| dirEntry "/dir/leaf")
        , test "/dir/leaf + /Dir updates dir node" <|
            \_ ->
                FileTree.fromString "/dir/leaf;/Dir"
                    |> get "/dir"
                    |> Expect.equal (Just <| dirEntry "/Dir")
        , test "/Dir + /dir/leaf + leaves dir node" <|
            \_ ->
                FileTree.fromString "/Dir;/dir/leaf"
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
        , test "map" <|
            \_ ->
                FileTree.fromString "/a/1;/a/2;/b"
                    -- |> FileTree.map (\e -> (fromEntries <| List.singleton <| dirEntry <| .path <| itemEntry e))
                    -- TODO replace identity by an actual test
                    |> FileTree.map identity
                    |> FileTree.toString
                    |> Expect.equal "/a;/a/1;/a/2;/b"
        , test "mapChildLists" <|
            \_ ->
                FileTree.fromString "/a/1;/a/2;/b"
                    |> mapChildLists (List.sortBy (itemEntry >> .path) >> List.take 1)
                    |> FileTree.toString
                    |> Expect.equal "/a;/a/1"
        , describe "combineSmallerEntries"
            [ test "combines top-level items" <|
                \_ ->
                    FileTree.fromString "/a:5;/b:4;/c:3;/d:2"
                        |> combineSmallerEntries 2 1
                        |> FileTree.toString
                        |> Expect.equal "/a:5;/b:4;/…2 smaller objects…:5"
            , test "combines non-toplevel items" <|
                \_ ->
                    FileTree.fromString "/a/1;/a/2;/a/3"
                        |> combineSmallerEntries 2 1
                        |> FileTree.toString
                        |> Expect.equal "/a;/a/1;/a/2;/a/…1 smaller object…:0"
            ]
        , test "trimDepth" <|
            \_ ->
                FileTree.fromString "/a;/a/b"
                    |> trimDepth 1
                    |> Expect.all
                        [ get "/a" >> Expect.equal (Just <| dirEntry "/a")
                        , get "/a/b" >> Expect.equal Nothing
                        ]
        ]


fromEntries : List FileEntry -> FileTree
fromEntries entries =
    FileTree.empty |> addEntries entries


dirEntry : String -> FileEntry
dirEntry path =
    FileEntry "folder" (String.toLower path) path Nothing


fileEntry : String -> Int -> FileEntry
fileEntry path size =
    FileEntry "file" (String.toLower path) path <| Just size
