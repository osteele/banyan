module DropboxExtrasTests exposing (..)

import Dropbox.Encoding exposing (..)
import Dropbox.Extras as DropboxExtras exposing (file, folder, info)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "FileTree" <|
        [ describe "decodeString"
            [ test "decodes file" <|
                \_ ->
                    decodeString "/a/b:12"
                        |> Expect.equal (file "/a/b" 12)
            , test "decodes zero-sized file" <|
                \_ ->
                    decodeString "/a/b"
                        |> Expect.equal (file "/a/b" 0)
            , test "decodes folder" <|
                \_ ->
                    decodeString "/a/b/"
                        |> Expect.equal (folder "/a/b")
            , test "decodes escape characters" <|
                \_ ->
                    decodeString "/a%3ab%3bc%25d/"
                        |> Expect.equal (folder "/a:b;c%d")
            , test "decodes relative files" <|
                \_ ->
                    decodeString "…/file"
                        |> Expect.all
                            [ info >> .name >> Expect.equal "file"
                            , info >> .pathDisplay >> Expect.equal Nothing
                            ]
            , test "decodes relative folders" <|
                \_ ->
                    decodeString "…/dir/"
                        |> Expect.all
                            [ info >> .name >> Expect.equal "dir"
                            , info >> .pathDisplay >> Expect.equal Nothing
                            ]
            ]
        , describe "encodeString"
            [ test "encodes file" <|
                \_ ->
                    file "/a/b" 12
                        |> encodeString
                        |> Expect.equal "/a/b:12"
            , test "encodes zero-size file" <|
                \_ ->
                    file "/a/b" 0
                        |> encodeString
                        |> Expect.equal "/a/b"
            , test "encodes folder" <|
                \_ ->
                    folder "/a/b"
                        |> encodeString
                        |> Expect.equal "/a/b/"
            , test "encodes escape characters" <|
                \_ ->
                    folder "/a:b;c%d"
                        |> encodeString
                        |> Expect.equal "/a%3ab%3bc%25d/"
            , test "encodes relative files" <|
                \_ ->
                    file "file" 0
                        |> encodeString
                        |> Expect.equal "…/file"
            , test "encodes relative folders" <|
                \_ ->
                    folder "folder"
                        |> encodeString
                        |> Expect.equal "…/folder/"
            ]
        , describe "encodeRelString"
            [ test "uses context" <|
                Expect.all
                    [ \_ ->
                        encodeRelString Nothing (file "/dir/a" 0)
                            |> Expect.equal ( Just "/dir/a", Nothing )
                    , \_ ->
                        encodeRelString (Just "/dir/") (file "/dir/a" 0)
                            |> Expect.equal ( Just "a", Just "/dir/" )
                    , \_ ->
                        encodeRelString (Just "/dir/") (file "/Dir/a" 0)
                            |> Expect.equal ( Just "a", Just "/dir/" )
                    , \_ ->
                        encodeRelString (Just "/dir/") (folder "/Dir/A")
                            |> Expect.equal ( Just "A/", Just "/dir/a/" )
                    ]
            ]
        , describe "decodeRelString"
            [ test "uses context" <|
                Expect.all
                    [ \_ ->
                        decodeRelString (Just "/dir/") "a/b"
                            |> Expect.equal (file "/dir/a/b" 0)
                    , \_ ->
                        decodeRelString (Just "/dir/") "/a/b"
                            |> Expect.equal (file "/a/b" 0)
                    ]
            ]
        ]
