port module TreeMap exposing (renderFileTreeMap)

import Dict
import Dropbox.FileTree exposing (..)
import Extras exposing (..)


type alias Node =
    { name : String
    , id : String
    , key : Maybe String
    , parent : Maybe String
    , value : Int
    }


port renderTreemap : ( String, List Node ) -> Cmd msg


renderFileTreeMap : Int -> FileTree -> Cmd msg
renderFileTreeMap _ tree =
    let
        path =
            nodePath tree

        title =
            dropPrefix "/" path |> Maybe.withDefault path
    in
        tree
            |> trimDepth 1
            |> combineSmallerEntries 10 2
            |> toNodeList
            |> curry renderTreemap title


toNodeList : FileTree -> List Node
toNodeList fileTree =
    let
        f : ( Maybe String, Int ) -> FileTree -> ( List Node, ( Maybe String, Int ) )
        f ( parentId, id ) item =
            let
                data =
                    itemData item

                idString =
                    Basics.toString id

                node =
                    { name = data.name
                    , id = idString
                    , key = ifJust (isFolder item) <| data.path
                    , parent = parentId
                    , value = nodeSize item
                    }

                ( childNodes, ( _, id2 ) ) =
                    Extras.flatMapS f ( Just idString, id ) <| Dict.values <| nodeChildren item
            in
                ( node :: childNodes, ( parentId, id2 ) )
    in
        Tuple.first <| Extras.flatMapS f ( Nothing, 0 ) <| Dict.values <| nodeChildren fileTree
