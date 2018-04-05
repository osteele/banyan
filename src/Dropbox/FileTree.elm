module Dropbox.FileTree
    exposing
        ( FileTree(..)
        , addEntries
        , addFromStrings
        , combineSmallerEntries
        , decoder
        , empty
        , encode
        , fromEntries
        , fromString
        , get
        , getSubtree
        , isFolder
        , isEmpty
        , itemData
        , itemEntry
        , map
        , mapChildLists
        , nodeChildren
        , nodeSize
        , nodePath
        , toListS
        , toString
        , trimDepth
        , logTree
        , logTrees
        )

{-|


## Hierarchical tree of Dropbox file and folder entries

See the official Dropbox documentation at
<https://www.dropbox.com/developers/documentation/http/documentation>

@docs FileTree


### Build

@docs empty, fromEntries


### Query

@docs get, getSubtree, isEmpty, isFolder


### Node Properties

@docs itemEntry, nodeChildren, nodeSize


### Mapping

@docs map, mapChildLists, mapChildren, toListS


### Transform

@docs combineSmallerEntries, trimDepth


### Serialization

@docs addFromStrings, fromString


### Update

@docs addEntries


### Debug

@docs fromString, toString

-}

import Dict
import Dropbox exposing (Metadata(..))
import Dropbox.Encoding exposing (..)
import Dropbox.Extras exposing (..)
import Extras exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


-- TYPES


{-| A Rose tree of FileEntry's, and cache rolled up sizes.

Note: Having a separate branch for the terminals (File) makes for more code but
less runtime data. I haven't measured whether this is worth it.

-}
type FileTree
    = File FileData
    | Folder FolderData Stats (Dict.Dict String FileTree)


type alias FileData =
    { name : String
    , path : String
    , size : Int
    }


type alias FolderData =
    { name : String
    , path : String
    }


{-| The rolled-up file stats. Currently just the size.
-}
type alias Stats =
    Int



-- BUILD


{-| Create an empty tree.
-}
empty : FileTree
empty =
    emptyNode ""


emptyNode : String -> FileTree
emptyNode path =
    Folder { name = takeFileName path, path = path } 0 Dict.empty


{-| Update a tree from file list metadata.
-}
addEntries : List Metadata -> FileTree -> FileTree
addEntries entries tree =
    let
        action entry =
            case entry of
                DeletedMeta _ ->
                    remove entry

                _ ->
                    insert entry
    in
        List.foldl action tree entries


fromEntries : List Metadata -> FileTree
fromEntries entries =
    empty |> addEntries entries



-- PREDICATES


{-| Determine if a dictionary is empty.
-}
isEmpty : FileTree -> Bool
isEmpty node =
    case node of
        File _ ->
            False

        Folder _ _ children ->
            Dict.isEmpty children


isFolder : FileTree -> Bool
isFolder node =
    case node of
        File _ ->
            False

        Folder _ _ _ ->
            True



-- QUERY


{-| Get the Dropbox metadata at with a file path.
-}
get : String -> FileTree -> Maybe Metadata
get path tree =
    getSubtree path tree |> Maybe.map itemEntry


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


itemEntry : FileTree -> Metadata
itemEntry tree =
    case tree of
        Folder { path } _ _ ->
            Dropbox.Extras.folder path

        File { path, size } ->
            Dropbox.Extras.file path size



-- NODE PROPERTIES


itemData : FileTree -> { name : String, path : String, size : Int }
itemData tree =
    case tree of
        File { name, path, size } ->
            { name = name, path = path, size = size }

        Folder { name, path } size _ ->
            { name = name, path = path, size = size }


nodeChildren : FileTree -> Dict.Dict String FileTree
nodeChildren tree =
    case tree of
        Folder _ _ children ->
            children

        _ ->
            Dict.empty


nodePath : FileTree -> String
nodePath item =
    case item of
        Folder { path } _ _ ->
            path

        File { path } ->
            path


nodeSize : FileTree -> Int
nodeSize tree =
    case tree of
        Folder _ size _ ->
            size

        File { size } ->
            size



-- INTERNAL


{-| Recompute a node's stats.
-}
recomputeStats : FileTree -> FileTree
recomputeStats tree =
    case tree of
        Folder data _ children ->
            let
                stats =
                    children |> Dict.values |> List.map nodeSize |> List.sum
            in
                Folder data stats children

        file ->
            file


updateTreeItem : List String -> (Maybe FileTree -> FileTree) -> List String -> FileTree -> FileTree
updateTreeItem keys alter path tree =
    let
        childAt k =
            emptyNode <| String.join "/" <| path ++ [ k ]

        -- construct a directory item for the current node, if not already present
        withDirItem : (FolderData -> Dict.Dict String FileTree -> a) -> a
        withDirItem fn =
            case tree of
                Folder data _ children ->
                    fn data children

                _ ->
                    let
                        p =
                            String.join "/" path
                    in
                        fn { name = takeFileName p, path = p } Dict.empty
    in
        case keys of
            [] ->
                alter <| Just tree

            [ k ] ->
                withDirItem <|
                    \entry children ->
                        Dict.update k (Just << alter) children
                            |> Folder entry 0
                            |> recomputeStats

            k :: ks ->
                let
                    alt =
                        updateTreeItem ks alter (path ++ [ k ])
                            << Maybe.withDefault (childAt k)
                in
                    withDirItem <|
                        \entry children ->
                            Dict.update k (Just << alt) children
                                |> Folder entry 0
                                |> recomputeStats


{-| Insert a value into a tree. Silently ignores data without pathDisplay.
-}
insert : Metadata -> FileTree -> FileTree
insert data =
    let
        updateFile d _ =
            File d

        updateDir d dir =
            case dir of
                Just (Folder _ size children) ->
                    Folder d size children

                _ ->
                    Folder d 0 Dict.empty

        -- return an update function, based on the type of data
        update path =
            case data of
                FileMeta { name, size } ->
                    updateFile { name = name, path = path, size = size }

                FolderMeta { name } ->
                    updateDir { name = name, path = path }

                _ ->
                    Debug.crash "unexpected Dropbox metadata case"
    in
        case Dropbox.Extras.info data |> .pathDisplay of
            Just path ->
                updateTreeItem (splitPath <| String.toLower path) (update path) [ "" ]

            _ ->
                identity


{-| Remove a value from a tree.
-}
remove : Metadata -> FileTree -> FileTree
remove meta =
    let
        rm : List String -> FileTree -> FileTree
        rm keys entry =
            case entry of
                File _ ->
                    empty

                Folder data stats children ->
                    case keys of
                        [] ->
                            empty

                        [ k ] ->
                            -- updateChildren (Dict.remove k) data
                            children |> Dict.remove k |> Folder data stats |> recomputeStats

                        k :: ks ->
                            children |> Dict.update k (Maybe.map <| rm ks) |> Folder data stats |> recomputeStats
    in
        case Dropbox.Extras.info meta |> .pathLower of
            Just path ->
                rm <| splitPath path

            Nothing ->
                identity



-- MORE INTERNALS


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


itemKey : FileTree -> String
itemKey tree =
    let
        path =
            itemData tree |> .path |> String.toLower
    in
        splitPath path |> List.reverse |> List.head |> Maybe.withDefault path



-- MAP


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
        Folder entry stats children ->
            children
                |> Dict.values
                |> fn
                |> List.map (\e -> ( itemKey e, e ))
                |> Dict.fromList
                |> Folder entry stats

        file ->
            file


{-| Apply fn to node child lists, in postfix order.
-}
mapChildLists : (List FileTree -> List FileTree) -> FileTree -> FileTree
mapChildLists fn =
    map <| mapChildList fn


{-| Fold the tree into a list, using a curried state monad function. Visit
nodes in prefix order.
-}
toListS : (s -> FileTree -> ( a, s )) -> s -> FileTree -> ( List a, s )
toListS f s item =
    let
        ( h, s1 ) =
            f s item

        ( t, s2 ) =
            flatMapS (toListS f) s1 <| Dict.values <| nodeChildren item
    in
        ( h :: t, s2 )



-- TRIM


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
                            |> \size ->
                                File { name = takeFileName name, path = name, size = size }
                in
                    List.take n sorted ++ [ combined ]


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



-- SERIALIZATION


addFromStrings : FileTree -> SerializationState -> List String -> ( FileTree, SerializationState )
addFromStrings tree s =
    let
        decodeEntry : SerializationState -> String -> ( Metadata, SerializationState )
        decodeEntry cwd =
            Dropbox.Encoding.decodeRelString cwd
                >> \entry ->
                    if isDir entry then
                        ( entry
                        , Dropbox.Extras.info entry
                            |> .pathLower
                            |> Maybe.map (\p -> p ++ "/")
                        )
                    else
                        ( entry, cwd )
    in
        mapS decodeEntry s
            >> Tuple.mapFirst (flip addEntries tree)


decodeFromString : SerializationState -> String -> ( FileTree, SerializationState )
decodeFromString s =
    String.split ";" >> addFromStrings empty s


encodeAsString : SerializationState -> FileTree -> ( String, SerializationState )
encodeAsString s =
    let
        encodeNode : SerializationState -> FileTree -> ( Maybe String, SerializationState )
        encodeNode s0 =
            itemEntry >> Dropbox.Encoding.encodeRelString s0
    in
        toListS encodeNode s
            >> Tuple.mapFirst
                (List.filterMap identity
                    >> -- the root folder is implicit
                       List.filter ((/=) "/")
                    >> String.join ";"
                )



-- TESTING AND DEBUGGING


{-| Construct a tree from a string.

The string is a ;-separated list of paths. Files end in :size, where size
is an optional size.

       fromString "/dir;/dir/a;/dir/b:10;/dir2/"
       fromString "/dir;a;b:10;/dir2/"

-}
fromString : String -> FileTree
fromString =
    decodeFromString Nothing >> Tuple.first


{-| Turn a tree into a string. See fromString for the format.
-}
toString : FileTree -> String
toString =
    encodeAsString Nothing >> Tuple.first


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



-- SERIALIZE


encode : FileTree -> Encode.Value
encode =
    toString >> Encode.string


decoder : Decoder FileTree
decoder =
    Decode.string |> Decode.andThen (Decode.succeed << fromString)
