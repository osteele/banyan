port module TreeMap exposing (..)

import Dict
import FileTree exposing (..)
import Json.Encode exposing (..)


port drawTreemap : Value -> Cmd msg


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
                    , ( "key", string key )
                    , ( "size", int size )
                    ]
            else
                object
                    [ ( "name", string name )
                    , ( "key", string key )
                    , ( "children", list <| List.map encode <| Dict.values children )
                    ]

        File { key, name, size } ->
            object
                [ ( "name", string name )
                , ( "key", string key )
                , ( "size", int <| Maybe.withDefault 0 size )
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
