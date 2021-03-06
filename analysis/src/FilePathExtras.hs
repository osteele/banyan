module FilePathExtras
  ( compareByDirectory
  , isDirectory
  , Relativizer
  , makeRelative
  , makeRelativeWithDots
  , makeRelativeWithMultipleDots
  , makeRelativeIfShorter
  ) where

import           Data.Function         ((&))
import           Data.List             (isPrefixOf, isSuffixOf, stripPrefix)
import           Data.Maybe            (fromMaybe)
import           ListExtras
import           System.FilePath.Posix

import           Text.Regex

{-| Same as `compare`, but sort files before subdirectories in the same
parent directory.
-}
compareByDirectory :: FilePath -> FilePath -> Ordering
compareByDirectory a b
  | da == db = compare a b
  | da `isPrefixOf` db = LT
  | db `isPrefixOf` da = GT
  | otherwise = compare a b
  where
    da = takeDirectory a
    db = takeDirectory b

{-| Is a path a directory name (a filepath that ends in POSIX `pathSeparator`)?

    isDirectory "/path/to" == False
    isDirectory "/path/to/" == True
-}
isDirectory :: FilePath -> Bool
isDirectory =
  isSuffixOf "/"

{-| Returns a file or directory path's parent dierctory.

   takeParentDirectory "a/b/c" == "a/b"
   takeParentDirectory "a/b/c/" == "a/b"
-}
takeParentDirectory :: FilePath -> FilePath
takeParentDirectory =
  addTrailingPathSeparator . takeDirectory . dropTrailingPathSeparator

{-|} A function that contracts a filename based on a reference path (the first
argument). `System.Filepath.makeRelative` is an example of a `Relativizer`.
-}
type Relativizer = FilePath -> FilePath -> FilePath

{-| Like `System.FilePath.Posix.makeRelative`, but uses `..`.

   makeRelativeWithDots "/a/" "/f" == "../f"
   makeRelativeWithDots "/a/b/" "/f" == "../../f"
-}
makeRelativeWithDots :: Relativizer
makeRelativeWithDots "." path = path
makeRelativeWithDots "/" path = makeRelative "/" path
makeRelativeWithDots base path =
  let parent = takeParentDirectory base
  in stripPrefix base path &
     fromMaybe (joinPath ["..", makeRelativeWithDots parent path])

{-| Like `makeRelativeWithDots`, but also uses `...`, `....` etc. to move up
multiple directory levels.

   makeRelativeWithDots "/a/" "/f" == "../f"
   makeRelativeWithDots "/a/b/" "/a/f" == "../f"
-}
makeRelativeWithMultipleDots :: Relativizer
makeRelativeWithMultipleDots base path =
  conjugateWithSentinels '/' (invariant combineAdjacentDots)
  $ makeRelativeWithDots base path
    where combineAdjacentDots s =
            subRegex (mkRegex "/(\\.\\.+)/\\.(\\.+)/") s "/\\1\\2/"

{-| Returns whichever of the original and relative path is shorter.

   makeRelativeIfShorter makeRelativeWithDots "/a/" "/f" == "/f"
   makeRelativeIfShorter makeRelativeWithDots "/a/b/c/" "/a/b/f" == "../b"
-}
makeRelativeIfShorter :: Relativizer -> Relativizer
makeRelativeIfShorter relativizer =
  shortest2 [original, relativizer]
    where original = flip const
