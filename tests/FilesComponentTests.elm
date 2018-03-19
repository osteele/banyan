module FilesComponentTests exposing (..)

import Expect exposing (Expectation)
import FilesComponent exposing (..)
import FileTree
import Test exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode


suite : Test
suite =
    describe "FilesComponent" <|
        [ describe "encodeStatus"
            [ test "FromCache" <|
                \_ ->
                    FromCache 695200000000
                        |> encodeStatus
                        |> toString
                        |> Expect.equal "\"<Sun Jan 12 1992 02:06:40 GMT-0500 (EST)>\""
            , test "other" <|
                \_ ->
                    Started
                        |> encodeStatus
                        |> toString
                        |> Expect.equal "null"
            ]
        , describe "statusDecoder"
            [ test "FromCache" <|
                \_ ->
                    "\"<Sun Jan 12 1992 02:06:40 GMT-0500 (EST)>\""
                        |> decodeString statusDecoder
                        |> Expect.equal (Result.Ok <| FromCache 695200000000)
            ]
        , test "encode" <|
            \_ ->
                FilesComponent.encode testModel
                    |> Encode.encode 0
                    |> Expect.equal testModelEncoding
        , test "decoder" <|
            \_ ->
                testModelEncoding
                    |> decodeString FilesComponent.decoder
                    |> Expect.equal (Result.Ok testModel)
        ]


testModel : FilesComponent.Model
testModel =
    let
        m =
            FilesComponent.init
    in
        { m
            | files = FileTree.fromString "/dir"
            , status = FromCache 695200000000
        }


testModelEncoding : String
testModelEncoding =
    """{"version":1,"files":"/dir","status":"<Sun Jan 12 1992 02:06:40 GMT-0500 (EST)>"}"""
