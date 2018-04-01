module FilePathExtras
    (isDirectory
    , makeRelativeWithDots) where

import Data.List
import Data.List.Split
import System.FilePath.Posix()

isDirectory :: FilePath -> Bool
isDirectory path =
    case reverse path of
        '/': _ -> True
        _ -> False

makeRelativeWithDots :: FilePath -> FilePath -> FilePath
makeRelativeWithDots base path =
    case stripPrefix base path of
        Just p -> p
        Nothing ->
            case reverse $ splitOn "/" base of
                _ : r ->
                    let
                        p = makeRelativeWithDots (intercalate "/" $ reverse r) path
                    in
                        "../" ++ p
