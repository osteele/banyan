module DropboxExtrasTests exposing (..)

import Dropbox
import DropboxExtras exposing (..)
import Expect exposing (Expectation)
import Extras exposing (takeFileName)
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import TestExtras exposing (..)


suite : Test
suite =
    describe "FileTree" <|
        [ describe "fromString"
            [ test "decodes file" <|
                \_ ->
                    DropboxExtras.decodeString "/a/b:12"
                        |> Expect.equal (file "b" "/a/b" 12)
            , test "decodes zero-sized file" <|
                \_ ->
                    DropboxExtras.decodeString "/a/b"
                        |> Expect.equal (file "b" "/a/b" 0)
            , test "decodes folder" <|
                \_ ->
                    DropboxExtras.decodeString "/a/b/"
                        |> Expect.equal (folder "/a/b")
            , test "encodes escape characters" <|
                \_ ->
                    DropboxExtras.decodeString "/a%3ab%3bc%25d/"
                        |> Expect.equal (folder "/a:b;c%d")
            ]
        , describe "toString"
            [ test "encodes file" <|
                \_ ->
                    file "b" "/a/b" 12
                        |> DropboxExtras.encodeString
                        |> Expect.equal "/a/b:12"
            , test "encodes zero-size file" <|
                \_ ->
                    file "a" "/a/b" 0
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
        ]
