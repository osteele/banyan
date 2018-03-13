module FileTree
    exposing
        ( FileTree(..)
        , addEntries
        , combineSmallerEntries
        , empty
        , encode
        , fromString
        , get
        , getSubtree
        , isEmpty
        , itemEntry
        , map
        , mapChildLists
        , nodeChildren
        , nodeSize
        , nodePath
        , toString
        , trimDepth
        )

{-|


## Hierarchical mdel of Dropbox files

See the official Dropbox documentation at
<https://www.dropbox.com/developers/documentation/http/documentation>

@docs FileTree


### Build

@docs empty, addEntries


### Query

@docs get, getSubtree, isEmpty


### Nodes

@docs itemEntry, nodeChildren, nodeSize


### Mapping

@docs map, mapChildLists, mapChildren


### Tree truncation

@docs combineSmallerEntries, trimDepth


### Debug

@docs fromString, toString

-}

import Dict
import FileEntry exposing (..)
import Json.Encode as Encode
import Utils exposing (..)


{-| A Rose tree of FileEntry's, and cache rolled up sizes.

Note: Having a separate branch for the terminals (File) makes for more code but
less runtime data. I haven't measured whether this is worth it.

-}
type FileTree
    = Dir FolderMetadata Stats (Dict.Dict String FileTree)
    | File FileMetadata


{-| The rolled-up file stats. Currently just the size.
-}
type alias Stats =
    Int


{-| Create an empty tree.
-}
empty : FileTree
empty =
    emptyNode ""


{-| Determine if a dictionary is empty.
-}
isEmpty : FileTree -> Bool
isEmpty tree =
    case tree of
        Dir _ _ children ->
            Dict.isEmpty children

        _ ->
            False


emptyNode : String -> FileTree
emptyNode name =
    Dir { key = name, name = takeFileName name, path = name } 0 Dict.empty


{-| Get the subtree at a file path.
-}
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


{-| Get the value associated with a file path.
-}
get : String -> FileTree -> Maybe FileEntry
get path tree =
    getSubtree path tree |> Maybe.map itemEntry


itemEntry : FileTree -> FileEntry
itemEntry tree =
    case tree of
        Dir data _ _ ->
            FileEntry.Folder data

        File data ->
            FileEntry.File data


nodeChildren : FileTree -> Dict.Dict String FileTree
nodeChildren tree =
    case tree of
        Dir _ _ children ->
            children

        _ ->
            Dict.empty


nodeKey : FileTree -> String
nodeKey item =
    case item of
        Dir { key } _ _ ->
            key

        File { key } ->
            key


nodePath : FileTree -> String
nodePath item =
    case item of
        Dir { path } _ _ ->
            path

        File { path } ->
            path


nodeSize : FileTree -> Int
nodeSize tree =
    case tree of
        Dir _ size _ ->
            size

        File { size } ->
            Maybe.withDefault 0 size


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
        withDirItem : (FolderMetadata -> Stats -> Dict.Dict String FileTree -> a) -> a
        withDirItem fn =
            case tree of
                Dir data size children ->
                    fn data size children

                _ ->
                    let
                        name =
                            String.join "/" path
                    in
                        fn { key = name, name = takeFileName name, path = name } 0 Dict.empty
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


{-| Insert a value into a tree.
-}
insert : FileEntry -> FileTree -> FileTree
insert entry =
    let
        updateFile data _ =
            File data

        updateDir data dir =
            case dir of
                Just (Dir _ size children) ->
                    Dir data size children

                _ ->
                    Dir data 0 Dict.empty

        update =
            case entry of
                FileEntry.File data ->
                    updateFile data

                FileEntry.Folder data ->
                    updateDir data

                _ ->
                    Debug.crash "unexpected FileEntry case"
    in
        updateTreeItem (splitPath <| FileEntry.key entry) update [ "" ]


{-| Remove a value from a tree.
-}
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
        rm <| splitPath <| FileEntry.key entry


{-| Update a tree from a list of values.
-}
addEntries : List FileEntry -> FileTree -> FileTree
addEntries entries tree =
    let
        action entry =
            case entry of
                FileEntry.Deleted _ ->
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
            nodeKey tree
    in
        splitPath key |> List.head |> Maybe.withDefault key


itemKeyTail : FileTree -> String
itemKeyTail tree =
    let
        key =
            nodeKey tree
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
                            |> Maybe.map nodePath
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
                            |> \size ->
                                File { key = name, name = takeFileName name, path = name, size = size }
                in
                    (List.take n sorted) ++ [ combined ]


{-| Remove leaves at depth > n from the root.
-}
trimDepth : Int -> FileTree -> FileTree
trimDepth n =
    mapChildList <|
        \children ->
            if n > 0 then
                List.map (trimDepth <| n - 1) children
            else
                []


{-| Construct a tree from a string. This is used in testing.

The string is a ;-separated list of paths. Files end in :size, where size
is an optional size.

       fromString "/dir;/dir/a:;/dir/b:10"

-}
fromString : String -> FileTree
fromString =
    String.split ";"
        >> List.map FileEntry.fromString
        >> flip addEntries empty


{-| Turn a tree into a string. See fromString for the format.
-}
toString : FileTree -> String
toString tree =
    let
        paths : FileTree -> List String
        paths tree =
            [ FileEntry.toString <| itemEntry tree ]
                ++ (nodeChildren tree |> Dict.values |> List.concatMap paths)
    in
        paths tree
            -- the root folder is implicit
            |> List.filter ((/=) "/")
            -- make the string deterministic
            |> List.sort
            |> String.join ";"


{-| Like Debug.log, but uses FileTree.toString to print the tree.
-}
logTree : String -> FileTree -> FileTree
logTree msg tree =
    let
        _ =
            Debug.log msg <| toString tree
    in
        tree


{-| Like Debug.log, but uses FileTree.toString to print the trees.
-}
logTrees : String -> List FileTree -> List FileTree
logTrees msg trees =
    let
        _ =
            trees
                |> List.map toString
                |> String.join ";"
                |> Debug.log msg
    in
        trees


encode : FileTree -> Encode.Value
encode tree =
    let
        enc t =
            case t of
                File { name, size } ->
                    Encode.object <|
                        [ ( "n", Encode.string name ) ]
                            ++ (Maybe.map (\s -> [ ( "s", Encode.int s ) ]) size
                                    |> Maybe.withDefault []
                               )

                Dir { name } _ children ->
                    Encode.object
                        [ ( "n", Encode.string name )
                        , ( "c", Encode.list <| List.map enc <| Dict.values children )
                        ]
    in
        Encode.object
            [ ( "version", Encode.int 1 )
            , ( "data", enc tree )
            ]
