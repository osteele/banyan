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
                        |> Expect.equal "\"1992-01-12T07:06:40.000Z\""
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
                    "\"1992-01-12T07:06:40.000Z\""
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
            , accountId = Just "1"
            , teamId = Just "2"
        }


testModelEncoding : String
testModelEncoding =
    """{"version":1,"files":"/dir","status":"1992-01-12T07:06:40.000Z","accountId":"1","teamId":"2"}"""
