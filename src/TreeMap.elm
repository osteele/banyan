port module TreeMap exposing (..)

import Dict
import FileEntry exposing (..)


type alias Node =
    { name : String
    , id : String
    , parent : Maybe String
    , value : Int
    }


port drawTreeMap : ( String, List Node ) -> Cmd msg


fileTreeMap : Int -> FileTree -> Cmd msg
fileTreeMap depth fileTree =
    let
        title =
            itemEntry fileTree |> .path
    in
    curry drawTreeMap title <| toNodes depth fileTree


toNodes : Int -> FileTree -> List Node
toNodes depth fileTree =
    let
        f : Maybe String -> ( Int, Int ) -> FileTree -> ( List Node, ( Int, Int ) )
        f parent ( nextId, depth ) item =
            let
                entry =
                    itemEntry item

                nodeId =
                    toString nextId

                node =
                    { name = entry.path
                    , id = nodeId
                    , parent = parent
                    , value = nodeSize item
                    }

                ( childNodes, ( nextId2, _ ) ) =
                    flatMapM (f <| Just nodeId) ( nextId, depth - 1 ) <| Dict.values <| nodeChildren item
            in
            if depth > 0 then
                ( node :: childNodes, ( nextId2, depth ) )
            else
                ( [], ( nextId, depth ) )
    in
    Tuple.first <| flatMapM (f Nothing) ( 0, depth ) (Dict.values <| nodeChildren fileTree)



-- flatMapM (f Nothing) 0 (Dict.values <| nodeChildren tt) |> Tuple.first
-- flatMap : (a -> List b) -> List a -> List b
-- flatMap f list =
--     List.map f list
--         |> List.foldr (++) []
-- flatTreeMapM :
-- flatTreeMapM =
-- flatTreeMapM : ( Maybe String, Int ) -> FileTree -> ( List Node, ( Maybe String, Int ) )
-- flatTreeMapM f s item =
--     let
--         (h, s2) = f s item
--         ( t, s3 ) =
--             flatMapM (flatTreeMapM f) ( Just nodeId, nextId + 1 ) <| Dict.values <| nodeChildren item
--     in
--     ( node :: childNodes, ( parent, nextId2 ) )


flatMapM : (s -> a -> ( List b, s )) -> s -> List a -> ( List b, s )
flatMapM f s xs =
    case xs of
        [] ->
            ( [], s )

        h :: t ->
            let
                ( r1, s2 ) =
                    f s h

                ( r2, s3 ) =
                    flatMapM f s2 t
            in
            ( r1 ++ r2, s3 )
