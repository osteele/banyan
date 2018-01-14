module FileEntry exposing (..)

import Dict


type alias FileEntry =
    { tag : String
    , key : String
    , path : String
    , size : Maybe Int
    }


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


emptyNode : String -> FileTree
emptyNode name =
    Dir (FileEntry "dir" name name Nothing) 0 Dict.empty


itemEntry : FileTree -> FileEntry
itemEntry item =
    case item of
        Dir e _ _ ->
            e

        File e ->
            e


nodeSize : FileTree -> Int
nodeSize item =
    case item of
        Dir _ n _ ->
            n

        File e ->
            e.size |> Maybe.withDefault 0


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
updateTreeItem ks alter path tree =
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
                    fn (FileEntry "dir" name name Nothing) 0 Dict.empty
    in
    case ks of
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


insert : String -> FileEntry -> FileTree -> FileTree
insert ks entry =
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
            if entry.tag == "dir" then
                updateDir
            else
                updateFile
    in
    updateTreeItem (splitPath entry.key) update [ "" ]


addEntries : List FileEntry -> FileTree -> FileTree
addEntries entries tree =
    List.foldl (\e -> insert e.key e) tree entries


splitPath : String -> List String
splitPath path =
    path
        |> dropPrefix "/"
        |> Maybe.withDefault path
        |> String.split "/"



-- utilities


dropPrefix : String -> String -> Maybe String
dropPrefix prefix s =
    if String.startsWith prefix s then
        s |> String.dropLeft (String.length prefix) |> Just
    else
        Nothing
