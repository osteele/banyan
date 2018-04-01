module Main where

import Data.List
import Data.List.Split
import Data.Semigroup ((<>))
import Options.Applicative
import System.Environment

import Serialize

data Options = Options
    { output      :: Maybe String
    , relative    :: Bool
    , stats       :: Bool
    , input       :: String
    }

options :: Parser Options
options = Options
    <$> optional (strOption
        ( long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Write output to FILE" ))
    <*> switch
        ( long "relative"
        <> short 'r'
        <> help "Use the ..-relative encoder" )
    <*> switch
        ( long "stats"
        <> short 's'
        <> help "Display statistics" )
    <*> argument str (metavar "FILE")

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (options <**> helper)
            ( fullDesc
            <> progDesc "Report path cache metrics"
            <> header "analyze - report path cache metrics" )

run :: Options -> IO ()
run (Options output relative stats infile) = do
    inString <- readFile infile >>= return . trimnl
    let encoder = if relative then encodePathsRel else encodePaths
    let inPaths = splitOn ";" inString
    let absPaths = decodePaths inPaths
    let outPaths = encoder absPaths
    let outString = intercalate ";" outPaths

    if stats then do
        putStrLn $ "input size : " ++ (show $ length inString)
        putStrLn $ "output size: " ++ (show $ length outString)
    else do
        putStrLn $ "input   : " ++ show inPaths
        putStrLn $ "decoded : " ++ show absPaths
        putStrLn $ "re-coded: " ++ show outPaths

    case output of
        Nothing -> do return ()
        Just output ->
            putStrLn $ intercalate ";" outPaths

trimnl =
    reverse . dropWhile (=='\n') . reverse
