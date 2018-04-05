module FilePathExtras
  ( isDirectory
  , makeRelativeWithDots
  , makeRelativeMultidots
  , makeShortestRelative
  , makeShortestMultidots
  ) where

import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import System.FilePath.Posix
import Text.Regex

import ListExtras

{-| Is a path a directory name (ends in pathSeparator)?

    isDirectory "/path/to" == False
    isDirectory "/path/to/" == True
-}
isDirectory :: FilePath -> Bool
isDirectory path =
  case reverse path of
    '/':_ -> True
    _ -> False

-- | A function that contracts a filename based on a reference path (the first
-- argument). `System.Filepath.makeRelative` is an example of a `Relativizer`.
type Relativizer = FilePath -> FilePath -> FilePath

-- | Like System.FilePath.Posix.makeRelative, but uses `..` wherever possible.
makeRelativeWithDots :: Relativizer
makeRelativeWithDots "." path = path
makeRelativeWithDots "/" path = makeRelative "/" path
makeRelativeWithDots base path =
  let parent = takeParentDirectory base
  in stripPrefix base path &
     fromMaybe (joinPath ["..", makeRelativeWithDots parent path])

-- | Like makeRelativeWithDots but uses `...`, `....` etc. to move up multiple
-- directory levels.
makeRelativeMultidots :: Relativizer
makeRelativeMultidots base path =
  withSentinel
    '/'
    (invariant $ \s -> subRegex (mkRegex "/(\\.\\.+)/\\.(\\.+)/") s "/\\1\\2/") $
  makeRelativeWithDots base path

-- | Return the shortest result from a list of relativizers.
shortestRelativizer :: [Relativizer] -> Relativizer
shortestRelativizer funcs base path = shortest (map uncurry funcs) (base, path)

-- | Like System.FilePath.Posix.makeRelative, but uses .. where this is shorter
-- than the non-relativized path.
makeShortestRelative :: Relativizer
makeShortestRelative = shortestRelativizer [flip const, makeRelativeWithDots]

-- | Returns whichever of the original and relative path is shorter. Uses ...
-- .... etc. to move up multiple directory levels.
makeShortestMultidots :: Relativizer
makeShortestMultidots = shortestRelativizer [flip const, makeRelativeMultidots]

{-|
   takeParentDirectory "a/b" ==> "a/"
   takeParentDirectory "a/b/" ==> "a/"
-}
takeParentDirectory :: FilePath -> FilePath
takeParentDirectory =
  addTrailingPathSeparator . takeDirectory . dropTrailingPathSeparator
