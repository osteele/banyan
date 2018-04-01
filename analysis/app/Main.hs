module Main where

import System.Environment
import Data.List
import Data.List.Split
import Serialize

main :: IO ()
main = do
    [filename] <- getArgs
    s <- readFile filename
    let relPaths = splitOn ";" $ trimnl s
    putStrLn $ "rel paths: " ++ show relPaths
    let absPaths = decodePaths relPaths
    putStrLn $ "abs paths: " ++ show absPaths
    putStrLn $ "reencoded: " ++ (show $ encodePaths absPaths)
    putStrLn $ "reencode2: " ++ (show $ encodePathsRel absPaths)
    -- putStrLn $ intercalate ";" reencoded

trimnl =
    reverse . dropWhile (=='\n') . reverse
