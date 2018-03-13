module FileTreeTests exposing (..)

import Expect exposing (Expectation)
import FileEntry exposing (..)
import FileTree exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "FileTree"
        [ describe "fromString"
            [ test "creates an unsized file" <|
                \_ ->
                    FileTree.fromString "/File"
                        |> get "/file"
                        >> expectJust (file "/File" Nothing)
            , test "creates a sized file" <|
                \_ ->
                    FileTree.fromString "/File:12"
                        |> get "/file"
                        >> expectJust (file "/File" <| Just 12)
            , test "creates a directory" <|
                \_ ->
                    FileTree.fromString "/Dir/"
                        |> get "/dir"
                        >> expectJust (folder "/Dir")
            , test "adds all the entries" <|
                \_ ->
                    FileTree.fromString "/dir/;/f1;/f2:10"
                        |> Expect.all
                            [ get "/dir" >> expectJust (folder "/dir")
                            , get "/f1" >> expectJust (file "/f1" Nothing)
                            , get "/f2" >> expectJust (file "/f2" <| Just 10)
                            ]
            ]
        , describe "toString"
            [ test "prints files, file sizes, and directories" <|
                \_ ->
                    FileTree.fromString "/a;/b/;/c:12"
                        |> FileTree.toString
                        |> Expect.equal "/a;/b/;/c:12"
            ]
        , describe "addEntries"
            [ test "/dir/ creates a directory node" <|
                \_ ->
                    FileTree.fromString "/dir/"
                        |> get "/dir"
                        |> expectJust (folder "/dir")
            , test "/dir/leaf creates the intermediate directory node" <|
                \_ ->
                    FileTree.fromString "/dir/leaf"
                        |> get "/dir"
                        |> expectJust (folder "/dir")
            , test "/dir/subdir/ creates a leaf node" <|
                \_ ->
                    FileTree.fromString "/dir/subdir/"
                        |> get "/dir/subdir"
                        |> expectJust (folder "/dir/subdir")
            , test "/dir/leaf + /Dir/ updates the directory name" <|
                \_ ->
                    FileTree.fromString "/dir/leaf;/Dir/"
                        |> get "/dir"
                        |> expectJust (folder "/Dir")
            , test "/dir/subdir/ + /Dir/ preserves the subdirectory" <|
                \_ ->
                    FileTree.fromString "/dir/subdir/;/Dir/"
                        |> get "/dir/subdir"
                        |> expectJust (folder "/dir/subdir")
            , test "/Dir/ + /dir/leaf + preserves the directory name" <|
                \_ ->
                    FileTree.fromString "/Dir/;/dir/leaf"
                        |> get "/dir"
                        |> expectJust (folder "/Dir")
            , test "delete /dir deletes the directory node" <|
                \_ ->
                    [ folder "/dir"
                    , FileEntry.Deletion { key = "/dir", path = "/dir" }
                    ]
                        |> fromEntries
                        |> get "/dir"
                        |> Expect.equal Nothing
            , test "delete /dir deletes the directory's children" <|
                \_ ->
                    [ folder "/dir"
                    , folder "/dir/subdir"
                    , FileEntry.Deletion { key = "/dir", path = "/dir" }
                    ]
                        |> fromEntries
                        |> get "/dir/subdir"
                        |> Expect.equal Nothing
            , test "delete /dir/leaf deletes only the leaf" <|
                \_ ->
                    [ folder "/dir"
                    , folder "/dir/leaf"
                    , FileEntry.Deletion { key = "/dir/leaf", path = "/dir/leaf" }
                    ]
                        |> fromEntries
                        |> Expect.all
                            [ get "/dir" >> expectJust (folder "/dir")
                            , get "/dir/child" >> Expect.equal Nothing
                            ]
            , test "/file deletes a previous directory's children" <|
                \_ ->
                    FileTree.fromString "/a/;/a/subdir;/a"
                        |> FileTree.toString
                        >> Expect.equal "/a"
            ]
        , test "map" <|
            \_ ->
                FileTree.fromString "/a/1;/a/2;/b"
                    -- |> FileTree.map (\e -> (fromEntries <| List.singleton <| folder <| itemPath e))
                    -- TODO replace identity by an actual test
                    |> FileTree.map identity
                    |> FileTree.toString
                    |> Expect.equal "/a/;/a/1;/a/2;/b"
        , test "mapChildLists" <|
            \_ ->
                FileTree.fromString "/a/1;/a/2;/b"
                    |> mapChildLists (List.sortBy nodePath >> List.take 1)
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
                    FileTree.fromString "/d/f1:5;/d/f2:4;/d/f3:3"
                        |> combineSmallerEntries 2 1
                        |> FileTree.toString
                        |> Expect.equal "/d/;/d/f1:5;/d/f2:4;/d/…1 smaller object…:3"
            ]
        , test "trimDepth" <|
            \_ ->
                FileTree.fromString "/a/;/b/c"
                    |> trimDepth 1
                    |> FileTree.toString
                    |> Expect.equal "/a/;/b/"
        ]


expectJust : a -> Maybe a -> Expectation
expectJust =
    Expect.equal << Just


fromEntries : List FileEntry -> FileTree
fromEntries entries =
    FileTree.empty |> addEntries entries
