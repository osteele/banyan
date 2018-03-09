module FileTreeTests exposing (..)

import Expect exposing (Expectation)
import FileEntry exposing (..)
import FileTree exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "FileTree"
        [ describe "fromString"
            [ test "creates files" <|
                \_ ->
                    FileTree.fromString "/a:12"
                        |> get "/a"
                        >> Expect.equal (Just <| fileEntry "/a" 12)
            , test "creates directories" <|
                \_ ->
                    FileTree.fromString "/dir/"
                        |> get "/dir"
                        >> Expect.equal (Just <| dirEntry "/dir")
            ]
        , describe "fromString >> toString"
            [ test "recognizes files, directories, and file sizes" <|
                \_ ->
                    FileTree.fromString "/a;/b/;/c:12"
                        |> FileTree.toString
                        |> Expect.equal "/a;/b/;/c:12"
            ]
        , test "/dir/ creates a directory node" <|
            \_ ->
                FileTree.fromString "/dir/"
                    |> get "/dir"
                    |> Expect.equal (Just <| dirEntry "/dir")
        , test "/dir/leaf creates a directory node" <|
            \_ ->
                FileTree.fromString "/dir/leaf"
                    |> get "/dir"
                    |> Expect.equal (Just <| dirEntry "/dir")
        , test "/dir/subdir/ creates a leaf node" <|
            \_ ->
                FileTree.fromString "/dir/subdir/"
                    |> get "/dir/subdir"
                    |> Expect.equal (Just <| dirEntry "/dir/subdir")
        , test "/dir/subdir/ + /Dir/ preserves the subdirectory" <|
            \_ ->
                FileTree.fromString "/dir/subdir/;/Dir/"
                    |> get "/dir/subdir"
                    |> Expect.equal (Just <| dirEntry "/dir/subdir")
        , test "/dir/leaf + /Dir/ updates the directory node" <|
            \_ ->
                FileTree.fromString "/dir/leaf;/Dir/"
                    |> get "/dir"
                    |> Expect.equal (Just <| dirEntry "/Dir")
        , test "/Dir/ + /dir/leaf + preserves the directory node" <|
            \_ ->
                FileTree.fromString "/Dir/;/dir/leaf"
                    |> get "/dir"
                    |> Expect.equal (Just <| dirEntry "/Dir")
        , test "delete /dir deletes the directory node" <|
            \_ ->
                [ dirEntry "/dir"
                , FileEntry.Deletion { key = "/dir", path = "/dir" }
                ]
                    |> fromEntries
                    |> get "/dir"
                    |> Expect.equal Nothing
        , test "delete /dir deletes the subdirectory node" <|
            \_ ->
                [ dirEntry "/dir"
                , dirEntry "/dir/subdir"
                , FileEntry.Deletion { key = "/dir", path = "/dir" }
                ]
                    |> fromEntries
                    |> get "/dir/subdir"
                    |> Expect.equal Nothing
        , test "delete /dir/subdir deletes the subdirectory node" <|
            \_ ->
                [ dirEntry "/dir"
                , dirEntry "/dir/subdir"
                , FileEntry.Deletion { key = "/dir/subdir", path = "/dir/subdir" }
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
                    |> Expect.equal "/a/;/a/1;/a/2;/b"
        , test "mapChildLists" <|
            \_ ->
                FileTree.fromString "/a/1;/a/2;/b"
                    |> mapChildLists (List.sortBy (itemEntry >> FileEntry.path) >> List.take 1)
                    |> FileTree.toString
                    |> Expect.equal "/a/;/a/1"
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
                        |> Expect.equal "/a/;/a/1;/a/2;/a/…1 smaller object…:0"
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
    FileEntry.Folder { key = String.toLower path, path = path }


fileEntry : String -> Int -> FileEntry
fileEntry path size =
    FileEntry.File { key = String.toLower path, path = path, size = Just size }
