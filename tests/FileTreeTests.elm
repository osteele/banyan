module FileTreeTests exposing (..)

import Dropbox
import DropboxExtras exposing (..)
import Expect exposing (Expectation)
import FileTree exposing (..)
import Test exposing (..)
import TestExtras exposing (..)


suite : Test
suite =
    describe "FileTree" <|
        [ describe "fromString"
            [ test "creates an unsized file" <|
                \_ ->
                    FileTree.fromString "/File"
                        |> get "/file"
                        >> expectJust (file "/File" 0)
            , test "creates a sized file" <|
                \_ ->
                    FileTree.fromString "/File:12"
                        |> get "/file"
                        >> expectJust (file "/File" 12)
            , test "creates a directory" <|
                \_ ->
                    FileTree.fromString "/Dir/"
                        |> get "/dir"
                        >> expectJust (folder "/Dir")
            , test "adds all the entries" <|
                \_ ->
                    FileTree.fromString "/dir/;/f1;/f2:12"
                        |> Expect.all
                            [ get "/dir" >> expectJust (folder "/dir")
                            , get "/f1" >> expectJust (file "/f1" 0)
                            , get "/f2" >> expectJust (file "/f2" 12)
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
                    , deleted "/dir"
                    ]
                        |> fromEntries
                        |> get "/dir"
                        |> Expect.equal Nothing
            , test "delete /dir deletes the directory's children" <|
                \_ ->
                    [ folder "/dir"
                    , folder "/dir/subdir"
                    , deleted "/dir"
                    ]
                        |> fromEntries
                        |> get "/dir/subdir"
                        |> Expect.equal Nothing
            , test "delete /dir/leaf deletes only the leaf" <|
                \_ ->
                    [ folder "/dir"
                    , folder "/dir/leaf"
                    , deleted "/dir/leaf"
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



-- ++ encoderTests
-- encoderTests : List Test
-- encoderTests =
--     let
--         encoded =
--             Encode.object
--                 [ ( "n", Encode.string "" )
--                 , ( "c"
--                   , Encode.list
--                         [ Encode.object
--                             [ ( "n", Encode.string "dir" )
--                             , ( "c", Encode.list [] )
--                             ]
--                         , Encode.object
--                             [ ( "n", Encode.string "file1" )
--                             ]
--                         , Encode.object
--                             [ ( "n", Encode.string "file2" )
--                             , ( "s", Encode.int 10 )
--                             ]
--                         ]
--                   )
--                 ]
--         encodedLeaf =
--             Encode.object
--                 [ ( "n", Encode.string "" )
--                 , ( "c"
--                   , Encode.list
--                         [ Encode.object
--                             [ ( "n", Encode.string "dir" )
--                             , ( "c"
--                               , Encode.list
--                                     [ Encode.object [ ( "n", Encode.string "file" ) ]
--                                     ]
--                               )
--                             ]
--                         ]
--                   )
--                 ]
--         encodedUppercase =
--             Encode.object
--                 [ ( "n", Encode.string "" )
--                 , ( "c"
--                   , Encode.list
--                         [ Encode.object
--                             [ ( "n", Encode.string "Dir" )
--                             , ( "c"
--                               , Encode.list
--                                     [ Encode.object [ ( "n", Encode.string "File" ) ]
--                                     ]
--                               )
--                             ]
--                         ]
--                   )
--                 ]
--     in
--         [ describe "encode"
--             [ test "files and directory" <|
--                 \_ ->
--                     FileTree.fromString "/file1;file2:10;/dir/"
--                         |> FileTree.encodeJson
--                         |> expectJsonEqual encoded
--             , test "file path" <|
--                 \_ ->
--                     FileTree.fromString "/dir/file"
--                         |> FileTree.encodeJson
--                         |> expectJsonEqual encodedLeaf
--             , pending test "uppercase name" <|
--                 \_ ->
--                     FileTree.fromString "/Dir/File"
--                         |> FileTree.encodeJson
--                         |> expectJsonEqual encodedUppercase
--             ]
--         , pending describe
--             "decode"
--             [ test "files and directory" <|
--                 \_ ->
--                     Encode.encode 0 encoded
--                         |> Decode.decodeString FileTree.jsonDecoder
--                         |> Result.map FileTree.toString
--                         |> Expect.equal (Result.Ok "")
--             ]
--         ]


fromEntries : List Dropbox.Metadata -> FileTree
fromEntries entries =
    FileTree.empty |> addEntries entries
