port module TreeMap exposing (..)

import Dict
import FileEntry
import FileTree exposing (..)
import Utils exposing (..)


type alias Node =
    { name : String
    , id : String
    , key : Maybe String
    , parent : Maybe String
    , value : Int
    }


port chart : ( String, List Node ) -> Cmd msg


renderFileTreeMap : Int -> FileTree -> Cmd msg
renderFileTreeMap _ tree =
    let
        path =
            itemEntry tree |> FileEntry.path

        title =
            dropPrefix "/" path |> Maybe.withDefault path
    in
        tree
            |> trimDepth 1
            |> combineSmallerEntries 10 2
            |> toNodes
            |> curry chart title


toNodes : FileTree -> List Node
toNodes fileTree =
    let
        f : ( Maybe String, Int ) -> FileTree -> ( List Node, ( Maybe String, Int ) )
        f ( parent, nextId1 ) item =
            let
                entry =
                    itemEntry item

                nodeId =
                    Basics.toString nextId1

                node =
                    { name = entry |> FileEntry.path |> Utils.takeFileName
                    , id = nodeId
                    , key = ifJust (FileEntry.isDir entry) <| FileEntry.key entry
                    , parent = parent
                    , value = nodeSize item
                    }

                ( childNodes, ( _, nextId2 ) ) =
                    flatMapM f ( Just nodeId, nextId1 ) <| Dict.values <| nodeChildren item
            in
                ( node :: childNodes, ( parent, nextId2 ) )
    in
        Tuple.first <| flatMapM f ( Nothing, 0 ) <| Dict.values <| nodeChildren fileTree



-- toNodes : FileTree -> List Node
-- toNodes fileTree =
--     let
--         f : ( Maybe String, Int ) -> FileTree -> ( List Node, ( Maybe String, Int ) )
--         f ( parent, nextId ) item =
--             let
--                 ( node, s2 ) =
--                     g ( parent, nextId ) item
--                 ( childNodes, ( _, nextId3 ) ) =
--                     flatMapM f s2 <| Dict.values <| nodeChildren item
--             in
--             ( node :: childNodes, ( parent, nextId3 ) )
--         g : ( Maybe String, Int ) -> FileTree -> ( Node, ( Maybe String, Int ) )
--         g ( parent, nextId ) item =
--             let
--                 entry =
--                     itemEntry item
--                 nodeId =
--                     toString nextId
--                 node =
--                     { name = entry.path |> Utils.takeFileName
--                     , id = nodeId
--                     , key = ifJust (isDir entry) entry.key
--                     , parent = parent
--                     , value = nodeSize item
--                     }
--             in
--             ( node, ( parent, nextId + 1 ) )
--     in
--     Tuple.first <| flatMapM f ( Nothing, 0 ) <| Dict.values <| nodeChildren fileTree


flatTreeMapM :
    (s -> FileTree -> ( Node, s ))
    -> s
    -> FileTree
    -> ( List Node, s )
flatTreeMapM f s item =
    let
        ( h, s2 ) =
            f s item

        ( t, s3 ) =
            flatMapM (flatTreeMapM f) s2 <| Dict.values <| nodeChildren item
    in
        ( h :: t, s3 )
