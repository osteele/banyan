module FilePathExtras
    (isDirectory
    , makeRelativeWithDots
    , makeRelativeMultidots
    , makeShortestRelative
    , makeShortestMultidots
    ) where

import Data.Function (on)
import Data.List

import System.FilePath.Posix
import Text.Regex

{-| Is a path a directory name (ends in pathSeparator)?

    isDirectory "/path/to" == False
    isDirectory "/path/to/" == True
-}
isDirectory :: FilePath -> Bool
isDirectory path =
    case reverse path of
        '/' : _ -> True
        _ -> False

-- | A function that contracts a filename based on a reference path (the first
-- argument). `System.Filepath.makeRelative` is an example of a `Relativizer`.
type Relativizer = FilePath -> FilePath -> FilePath

-- | Like System.FilePath.Posix.makeRelative, but uses `..` wherever possible.
makeRelativeWithDots :: Relativizer
makeRelativeWithDots "." path = path
makeRelativeWithDots "/" path = makeRelative "/" path
makeRelativeWithDots base path =
    case stripPrefix base path of
        Just rel -> rel -- ++ "-j(" ++ base ++ ")"
        Nothing ->
            let parent = takeParentDirectory base
            in joinPath ["..", makeRelativeWithDots parent path]


-- | Like makeRelativeWithDots but uses `...`, `....` etc. to move up multiple
-- directory levels.
makeRelativeMultidots :: Relativizer
makeRelativeMultidots base path =
    withSentinel '/' (fixConverge $ \s -> subRegex (mkRegex "/(\\.\\.+)/\\.(\\.+)/") s "/\\1\\2/")
    $ makeRelativeWithDots base path


-- | Apply each function to a value; return the shortest result
shortest :: Foldable f => [a -> f b] -> a -> f b
shortest funcs x =
    minimumBy (compare `on` length) $ (funcs <*> [x])


-- | Return the shortest result from a list of relativizers.
shortestRelativizer :: [Relativizer] -> Relativizer
shortestRelativizer funcs base path =
    shortest (map uncurry funcs) (base, path)


-- | Like System.FilePath.Posix.makeRelative, but uses .. where this is shorter
-- than the non-relativized path.
makeShortestRelative :: Relativizer
makeShortestRelative  =
    shortestRelativizer [flip const,  makeRelativeWithDots]


-- | Returns whichever of the original and relative path is shorter. Uses ...
-- .... etc. to move up multiple directory levels.
makeShortestMultidots :: Relativizer
makeShortestMultidots =
    shortestRelativizer [flip const,  makeRelativeMultidots]

-- | Cf. `Data.function.fix`, which returns the *least-defined* fixed point.
fixConverge :: Eq a => (a -> a) -> a -> a
fixConverge fn a =
    let b = fn a
    in if a == b then a else fn b


{-|
   takeParentDirectory "a/b" ==> "a/"
   takeParentDirectory "a/b/" ==> "a/"
-}
takeParentDirectory :: FilePath -> FilePath
takeParentDirectory =
    addTrailingPathSeparator . takeDirectory . dropTrailingPathSeparator


-- | Append a to each end of a list, applies the function, and removes the
-- first and last element of the result.
withSentinel :: a -> ([a] -> [b]) -> [a] -> [b]
withSentinel s func =
    let eachEnd f = f . reverse . f . reverse
    in eachEnd tail . func . eachEnd (s :)
