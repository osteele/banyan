port module TreeMap exposing (..)

import Dict
import FileTree exposing (..)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class, href, id, style)
import Json.Encode exposing (..)
import Svg exposing (svg)


port drawTreemap : Value -> Cmd msg


treeMap : m -> Html msg
treeMap _ =
    svg [ id "treeMap" ] []


renderFileTreeMap : Int -> FileTree -> Cmd msg
renderFileTreeMap depth tree =
    tree
        |> trimDepth depth
        |> combineSmallerEntries 10 2
        |> encode
        |> drawTreemap


encode : FileTree -> Value
encode tree =
    case tree of
        Dir { key, name } size children ->
            if Dict.isEmpty children then
                object
                    [ ( "name", string name )
                    , ( "id", string key )
                    , ( "size", int size )
                    ]
            else
                object
                    [ ( "name", string name )
                    , ( "id", string key )
                    , ( "size", int size )
                    , ( "children", list <| List.map encode <| Dict.values children )
                    ]

        File { key, name, size } ->
            object
                [ ( "name", string name )
                , ( "id", string key )
                , ( "size", int size )
                ]



-- flatTreeMapM :
--     (s -> FileTree -> ( Node, s ))
--     -> s
--     -> FileTree
--     -> ( List Node, s )
-- flatTreeMapM f s item =
--     let
--         ( h, s2 ) =
--             f s item
--         ( t, s3 ) =
--             flatMapM (flatTreeMapM f) s2 <| Dict.values <| nodeChildren item
--     in
--         ( h :: t, s3 )
