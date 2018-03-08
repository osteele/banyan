module FileTree
    exposing
        ( FileTree(..)
        , addEntries
        , combineSmallerEntries
        , empty
        , getSubtree
        , isEmpty
        , itemEntry
        , nodeChildren
        , nodeSize
        , trimDepth
        , trimTree
        , fromDebugString
        , toDebugString
        , get
        , map
        , mapChildLists
        )

{-|


## Hierarchical mdel of Dropbox files

See the official Dropbox documentation at
<https://www.dropbox.com/developers/documentation/http/documentation>

@docs FileTree


### Build

@docs empty, addEntries


### Query

@docs getSubtree, isEmpty


### Nodes

@docs itemEntry, nodeChildren, nodeSize


### Tree truncation

@docs trimDepth, trimTree, combineSmallerEntries


### Debug

@docs fromDebugString, toDebugString

-}

import Dict
import FileEntry exposing (..)
import Utils exposing (..)


{-| A Rose tree of FileEntry's, and cache rolled up sizes.

Note: Having a separate branch for the terminals (File) makes for more code but
less runtime data. I haven't measured whether this is worth it.

-}
type FileTree
    = Dir FileEntry Stats (Dict.Dict String FileTree)
    | File FileEntry


{-| The rolled-up file stats. Currently just the size.
-}
type alias Stats =
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
        Dir data stats children ->
            Dir data stats <|
                if depth > 0 then
                    mapValues (trimTree <| depth - 1) children
                else
                    Dict.empty

        leaf ->
            leaf


{-| Recompute a node's stats.
-}
recomputeStats : FileTree -> FileTree
recomputeStats tree =
    case tree of
        Dir e _ children ->
            let
                stats =
                    children |> Dict.values |> List.map nodeSize |> List.sum
            in
                Dir e stats children

        leaf ->
            leaf


updateTreeItem : List String -> (Maybe FileTree -> FileTree) -> List String -> FileTree -> FileTree
updateTreeItem keys alter path tree =
    let
        childAt k =
            emptyNode <| String.join "/" <| path ++ [ k ]

        -- construct a directory item for the current node, if not already present
        withDirItem : (FileEntry -> Stats -> Dict.Dict String FileTree -> a) -> a
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
                            |> recomputeStats

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
                                |> recomputeStats


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


remove : FileEntry -> FileTree -> FileTree
remove entry =
    let
        updateChildren update entry =
            case entry of
                File _ ->
                    entry

                Dir entry stats children ->
                    children |> update |> Dir entry stats |> recomputeStats

        rm : List String -> FileTree -> FileTree
        rm keys entry =
            case entry of
                File _ ->
                    empty

                Dir entry stats children ->
                    case keys of
                        [] ->
                            empty

                        [ k ] ->
                            -- updateChildren (Dict.remove k) entry
                            children |> Dict.remove k |> Dir entry stats |> recomputeStats

                        k :: ks ->
                            children |> Dict.update k (Maybe.map <| rm ks) |> Dir entry stats |> recomputeStats
    in
        rm <| splitPath entry.key


addEntries : List FileEntry -> FileTree -> FileTree
addEntries entries tree =
    let
        action entry =
            case entry.tag of
                "deleted" ->
                    remove entry

                _ ->
                    insert entry
    in
        List.foldl action tree entries


dirname : String -> String
dirname path =
    path
        |> String.split "/"
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> String.join "/"


splitPath : String -> List String
splitPath path =
    path
        |> dropPrefix "/"
        |> Maybe.withDefault path
        |> String.split "/"


itemKeyHead : FileTree -> String
itemKeyHead tree =
    let
        key =
            itemEntry tree |> .key
    in
        splitPath key |> List.head |> Maybe.withDefault key


itemKeyTail : FileTree -> String
itemKeyTail tree =
    let
        key =
            itemEntry tree |> .key
    in
        splitPath key |> List.reverse |> List.head |> Maybe.withDefault key


{-| Apply fn to the tree's nodes in postfix order. Don't recompute the statss.
-}
map : (FileTree -> FileTree) -> FileTree -> FileTree
map fn =
    mapChildList (List.map <| map fn) >> fn


{-| Apply fn to the node's child list.
-}
mapChildList : (List FileTree -> List FileTree) -> FileTree -> FileTree
mapChildList fn tree =
    case tree of
        Dir entry stats children ->
            children
                |> Dict.values
                |> fn
                |> List.map (\e -> ( itemKeyTail e, e ))
                |> Dict.fromList
                |> Dir entry stats

        leaf ->
            leaf


{-| Apply fn to node child lists, in postfix order.
-}
mapChildLists : (List FileTree -> List FileTree) -> FileTree -> FileTree
mapChildLists fn =
    map <| mapChildList fn


{-| Combine any children after the first n, ordered by descending size, into a
single child, if the new child would combine at least orphans number of items.
This function applies recursively.
-}
combineSmallerEntries : Int -> Int -> FileTree -> FileTree
combineSmallerEntries n orphans =
    mapChildLists <|
        \children ->
            if List.length children < (n + orphans) then
                children
            else
                let
                    parentKey =
                        List.head children
                            |> Maybe.map (itemEntry >> .path)
                            |> Maybe.withDefault "/"
                            |> dirname

                    name =
                        String.join ""
                            [ parentKey
                            , "/…"
                            , quantify "smaller object" <| List.length children - n
                            , "…"
                            ]

                    sorted =
                        children
                            |> List.sortBy (nodeSize >> negate)

                    combined =
                        List.drop n sorted
                            |> List.map nodeSize
                            |> List.sum
                            |> Just
                            |> FileEntry.FileEntry FileEntry.fileTag name name
                            |> File
                in
                    (List.take n sorted) ++ [ combined ]


{-| Remove leaves at depth > n from the root.
-}
trimDepth : Int -> FileTree -> FileTree
trimDepth n tree =
    case tree of
        Dir entry stats children ->
            let
                children_ =
                    if n > 0 then
                        Dict.map (\_ -> trimDepth (n - 1)) children
                    else
                        Dict.empty
            in
                Dir entry stats children_

        leaf ->
            leaf


fromDebugString : String -> FileTree
fromDebugString =
    let
        pathToEntry p =
            FileEntry dirTag (String.toLower p) p Nothing
    in
        String.split ";"
            >> List.map pathToEntry
            >> flip addEntries empty


toDebugString : FileTree -> String
toDebugString t =
    let
        paths : FileTree -> List (List String)
        paths t =
            let
                key =
                    itemEntry t |> .path
            in
                [ [ key ] ]
                    ++ (nodeChildren t
                            |> Dict.values
                            |> List.map paths
                            |> List.concat
                       )
    in
        paths t
            |> List.map (String.join "/")
            |> List.filter ((/=) "")
            |> List.sort
            |> String.join ";"


logTree : String -> FileTree -> FileTree
logTree msg t =
    let
        _ =
            Debug.log msg <| toDebugString t
    in
        t


logTrees : String -> List FileTree -> List FileTree
logTrees msg ts =
    let
        _ =
            ts
                |> List.map toDebugString
                |> String.join ";"
                |> Debug.log msg
    in
        ts
