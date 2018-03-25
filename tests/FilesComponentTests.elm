module FilesComponentTests exposing (..)

import Dropbox.FileTree as FileTree
import Expect exposing (Expectation)
import FilesComponent exposing (..)
import Test exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode


suite : Test
suite =
    describe "FilesComponent" <|
        [ describe "encodeState"
            [ test "FromCache" <|
                \_ ->
                    FromCache 695200000000
                        |> encodeState
                        |> toString
                        |> Expect.equal "\"1992-01-12T07:06:40.000Z\""
            , test "other" <|
                \_ ->
                    Started
                        |> encodeState
                        |> toString
                        |> Expect.equal "null"
            ]
        , describe "stateDecoder"
            [ test "FromCache" <|
                \_ ->
                    "\"1992-01-12T07:06:40.000Z\""
                        |> decodeString stateDecoder
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
            , state = FromCache 695200000000
            , accountId = Just "1"
            , teamId = Just "2"
        }


testModelEncoding : String
testModelEncoding =
    """{"version":1,"files":"/dir","state":"1992-01-12T07:06:40.000Z","accountId":"1","teamId":"2"}"""
