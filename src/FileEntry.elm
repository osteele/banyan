module FileEntry exposing (..)

import Dict


type alias FileEntry =
    { tag : String
    , key : String
    , path : String
    , size : Maybe Int
    }


type FileTree
    = Dir FileEntry Int (Dict.Dict String FileTree)
    | File FileEntry


emptyFileTree : String -> FileTree
emptyFileTree name =
    Dir (FileEntry "dir" name name Nothing) 0 Dict.empty


itemEntry : FileTree -> FileEntry
itemEntry item =
    case item of
        Dir e _ _ ->
            e

        File e ->
            e


itemSize : FileTree -> Int
itemSize item =
    case item of
        Dir _ n _ ->
            n

        File e ->
            e.size |> Maybe.withDefault 0


recalcSize : FileTree -> FileTree
recalcSize tree =
    case tree of
        Dir e _ children ->
            let
                s =
                    children |> Dict.values |> List.map itemSize |> List.sum
            in
            Dir e s children

        file ->
            file


updateTreeItem : List String -> (Maybe FileTree -> FileTree) -> List String -> FileTree -> FileTree
updateTreeItem ks alter path tree =
    let
        hereNode =
            let
                name =
                    String.join "/" path
            in
            FileEntry "dir" name name Nothing

        subNode next =
            let
                name =
                    String.join "/" (path ++ [ next ])
            in
            FileEntry "dir" name name Nothing

        withDirItem fn =
            case tree of
                Dir entry size children ->
                    fn entry size children

                _ ->
                    fn hereNode 0 Dict.empty

        df k =
            Maybe.withDefault (Dir (subNode k) 0 Dict.empty)
    in
    case ks of
        [] ->
            alter <| Just tree

        k :: [] ->
            withDirItem <|
                \entry _ children ->
                    Dict.update k (Just << alter) children |> Dir entry 0 |> recalcSize

        k :: ks ->
            withDirItem <|
                \entry _ children ->
                    Dict.update k (Just << (\t -> updateTreeItem ks alter (path ++ [ k ]) (df k t))) children
                        |> Dir entry 0
                        |> recalcSize


insertFileEntry : List String -> FileEntry -> FileTree -> FileTree
insertFileEntry ks entry =
    let
        updateFile _ =
            File entry

        updateDir dir =
            case dir of
                Just (Dir e size children) ->
                    Dir entry size children

                _ ->
                    Dir entry 0 Dict.empty
    in
    updateTreeItem ks
        (if entry.tag == "dir" then
            updateDir
         else
            updateFile
        )
        []


addFileEntries : List FileEntry -> FileTree -> FileTree
addFileEntries entries tree =
    let
        components path =
            path |> dropPrefix "/" |> Maybe.withDefault path |> String.split "/"

        addEntry entry =
            insertFileEntry (components entry.key) entry
    in
    List.foldl addEntry tree entries



-- utilities


dropPrefix : String -> String -> Maybe String
dropPrefix prefix s =
    if String.startsWith prefix s then
        s |> String.dropLeft (String.length prefix) |> Just
    else
        Nothing
