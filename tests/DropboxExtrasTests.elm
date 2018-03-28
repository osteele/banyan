module DropboxExtrasTests exposing (..)

import Dropbox.Extras as DropboxExtras exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "FileTree" <|
        [ describe "decodeString"
            [ test "decodes file" <|
                \_ ->
                    DropboxExtras.decodeString "/a/b:12"
                        |> Expect.equal (file "/a/b" 12)
            , test "decodes zero-sized file" <|
                \_ ->
                    DropboxExtras.decodeString "/a/b"
                        |> Expect.equal (file "/a/b" 0)
            , test "decodes folder" <|
                \_ ->
                    DropboxExtras.decodeString "/a/b/"
                        |> Expect.equal (folder "/a/b")
            , test "encodes escape characters" <|
                \_ ->
                    DropboxExtras.decodeString "/a%3ab%3bc%25d/"
                        |> Expect.equal (folder "/a:b;c%d")
            ]
        , describe "encodeString"
            [ test "encodes file" <|
                \_ ->
                    file "/a/b" 12
                        |> DropboxExtras.encodeString
                        |> Expect.equal "/a/b:12"
            , test "encodes zero-size file" <|
                \_ ->
                    file "/a/b" 0
                        |> DropboxExtras.encodeString
                        |> Expect.equal "/a/b"
            , test "encodes folder" <|
                \_ ->
                    folder "/a/b"
                        |> DropboxExtras.encodeString
                        |> Expect.equal "/a/b/"
            , test "encodes escape characters" <|
                \_ ->
                    folder "/a:b;c%d"
                        |> DropboxExtras.encodeString
                        |> Expect.equal "/a%3ab%3bc%25d/"
            ]
        , describe "encodeRelString"
            [ test "uses context" <|
                Expect.all
                    [ \_ ->
                        DropboxExtras.encodeRelString Nothing (file "/dir/a" 0)
                            |> Expect.equal ( Just "/dir/a", Nothing )
                    , \_ ->
                        DropboxExtras.encodeRelString (Just "/dir/") (file "/dir/a" 0)
                            |> Expect.equal ( Just "a", Just "/dir/" )
                    , \_ ->
                        DropboxExtras.encodeRelString (Just "/dir/") (file "/Dir/a" 0)
                            |> Expect.equal ( Just "a", Just "/dir/" )
                    , \_ ->
                        DropboxExtras.encodeRelString (Just "/dir/") (folder "/Dir/A")
                            |> Expect.equal ( Just "A/", Just "/dir/a/" )
                    ]
            ]
        , describe "decodeRelString"
            [ test "uses context" <|
                Expect.all
                    [ \_ ->
                        DropboxExtras.decodeRelString (Just "/dir/") "a/b"
                            |> Expect.equal (file "/dir/a/b" 0)
                    , \_ ->
                        DropboxExtras.decodeRelString (Just "/dir/") "/a/b"
                            |> Expect.equal (file "/a/b" 0)
                    ]
            ]
        ]
