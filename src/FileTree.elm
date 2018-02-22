module FileTree exposing (..)

import Dict
import FileEntry exposing (..)
import Utils exposing (..)


{-| A Rose tree of FileEntry's, and cached rolled up sizes.

Note: Having a separate branch for the terminals (File) makes for more code but
less runtime data. I haven't measured whether this is worth it.

-}
type FileTree
    = Dir FileEntry FileNodeCache (Dict.Dict String FileTree)
    | File FileEntry


{-| The rolled-up file size.
-}
type alias FileNodeCache =
    Int


empty : FileTree
empty =
    emptyNode ""


isEmpty : FileTree -> Bool
isEmpty tree =
    case tree of
        Dir _ _ children ->
            Dict.isEmpty children

        _ ->
            False


emptyNode : String -> FileTree
emptyNode name =
    Dir (FileEntry dirTag name name Nothing) 0 Dict.empty


getSubtree : String -> FileTree -> Maybe FileTree
getSubtree path tree =
    let
        get_ : List String -> FileTree -> Maybe FileTree
        get_ p t =
            case p of
                [] ->
                    Just <| t

                d :: [] ->
                    nodeChildren t |> Dict.get d

                d :: pt ->
                    nodeChildren t |> Dict.get d |> Maybe.andThen (get_ pt)
    in
        get_ (splitPath <| String.toLower path) tree


get : String -> FileTree -> Maybe FileEntry
get path tree =
    getSubtree path tree |> Maybe.map itemEntry


itemEntry : FileTree -> FileEntry
itemEntry item =
    case item of
        Dir e _ _ ->
            e

        File e ->
            e


nodeChildren : FileTree -> Dict.Dict String FileTree
nodeChildren tree =
    case tree of
        Dir _ _ children ->
            children

        _ ->
            Dict.empty


nodeSize : FileTree -> Int
nodeSize item =
    case item of
        Dir _ n _ ->
            n

        File e ->
            e.size |> Maybe.withDefault 0


{-| Trim the tree to a maximum depth.
-}
trimTree : Int -> FileTree -> FileTree
trimTree depth tree =
    case tree of
        Dir data cache children ->
            Dir data cache <|
                if depth > 0 then
                    mapValues (trimTree <| depth - 1) children
                else
                    Dict.empty

        _ ->
            tree


{-| Recompute a node's cache fields.
-}
updateCache : FileTree -> FileTree
updateCache tree =
    case tree of
        Dir e _ children ->
            let
                s =
                    children |> Dict.values |> List.map nodeSize |> List.sum
            in
                Dir e s children

        _ ->
            tree


updateTreeItem : List String -> (Maybe FileTree -> FileTree) -> List String -> FileTree -> FileTree
updateTreeItem keys alter path tree =
    let
        childAt k =
            emptyNode <| String.join "/" <| path ++ [ k ]

        -- construct a directory item for the current node, if not already present
        withDirItem : (FileEntry -> FileNodeCache -> Dict.Dict String FileTree -> a) -> a
        withDirItem fn =
            case tree of
                Dir entry size children ->
                    fn entry size children

                _ ->
                    let
                        name =
                            String.join "/" path
                    in
                        fn (FileEntry dirTag name name Nothing) 0 Dict.empty
    in
        case keys of
            [] ->
                alter <| Just tree

            k :: [] ->
                withDirItem <|
                    \entry _ children ->
                        Dict.update k (Just << alter) children
                            |> Dir entry 0
                            |> updateCache

            k :: ks ->
                let
                    alt =
                        updateTreeItem ks alter (path ++ [ k ])
                            << Maybe.withDefault (childAt k)
                in
                    withDirItem <|
                        \entry _ children ->
                            Dict.update k (Just << alt) children
                                |> Dir entry 0
                                |> updateCache


insert : FileEntry -> FileTree -> FileTree
insert entry =
    let
        updateFile _ =
            File entry

        updateDir dir =
            case dir of
                Just (Dir _ size children) ->
                    Dir entry size children

                _ ->
                    Dir entry 0 Dict.empty

        update =
            if entry.tag == dirTag then
                updateDir
            else
                updateFile
    in
        updateTreeItem (splitPath entry.key) update [ "" ]


addEntries : List FileEntry -> FileTree -> FileTree
addEntries entries tree =
    List.foldl insert tree entries


splitPath : String -> List String
splitPath path =
    path
        |> dropPrefix "/"
        |> Maybe.withDefault path
        |> String.split "/"
