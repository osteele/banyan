module FilePathExtras
    (isDirectory
    , makeRelativeWithDots
    , makeRelativeMultidots
    , makeShortestRelative
    , makeShortestMultidots
    ) where

import Data.Function (on)
import Data.List
import Data.List.Utils (replace)
import System.FilePath.Posix

isDirectory :: FilePath -> Bool
isDirectory path =
    case reverse path of
        '/': _ -> True
        _ -> False

-- Like System.FilePath.Posix.makeRelative, but uses .. wherever possible
makeRelativeWithDots :: FilePath -> FilePath -> FilePath
makeRelativeWithDots "." path = path
makeRelativeWithDots "/" path = makeRelative "/" path
makeRelativeWithDots base path =
    case stripPrefix base path of
        Just rel -> rel -- ++ "-j(" ++ base ++ ")"
        Nothing ->
            let parent = takeParentDirectory base
            in joinPath ["..", makeRelativeWithDots parent path]

-- Like System.FilePath.Posix.makeRelative, but uses .. where this is shorter
-- than the non-relativized path.
makeShortestRelative :: FilePath -> FilePath -> FilePath
makeShortestRelative base path =
    minimumBy (compare `on` length) [path, makeRelativeWithDots base path]

-- TODO add sentinels; require / at ends
makeRelativeMultidots :: FilePath -> FilePath -> FilePath
makeRelativeMultidots base path =
    replace "../.." "..."
    $ replace "../.." "..."
    $ replace "../.." "..."
    $ makeRelativeWithDots base path

makeShortestMultidots :: FilePath -> FilePath -> FilePath
makeShortestMultidots base path =
    minimumBy (compare `on` length) [path, makeRelativeMultidots base path]

-- takeParentDirectory "a/b" ==> "a/"
-- takeParentDirectory "a/b/" ==> "a/"
takeParentDirectory :: FilePath -> FilePath
takeParentDirectory =
    addTrailingPathSeparator . takeDirectory . dropTrailingPathSeparator
