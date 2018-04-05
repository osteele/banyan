module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Semigroup ((<>))
import Options.Applicative
import System.Environment

import Serialize

data Options = Options
    { stats       :: Bool
    , useDots     :: Bool
    , combineDots :: Bool
    , output      :: Maybe String
    , input       :: String
    }

options :: Parser Options
options = Options
    <$> switch
        ( long "stats"
        <> short 's'
        <> help "Display statistics" )
    <*> switch
        ( long "use-dots"
        <> short 'd'
        <> help "Allow .. in relative pathnames" )
    <*> switch
        ( long "combine-dots"
        <> help "Allow ... etc. in pathnames" )
    <*> optional (strOption
        ( long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Write output to FILE" ))
    <*> argument str (metavar "FILE")

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (options <**> helper)
            ( fullDesc
            <> progDesc "Report path cache metrics"
            <> header "analyze - report path cache metrics" )

run :: Options -> IO ()
run (Options stats dots multidots output infile) = do
    inString <- trimnl <$> readFile infile
    let
        encoder =
            case (dots, multidots) of
                (False, _) -> encodePaths
                (True, False) -> encodePathsRel
                (True, True)  -> encodePathsMultidot
    let inPaths = splitOn entryPathSeparator inString
    let absPaths = decodePaths inPaths
    let outPaths = encoder absPaths
    let outString = intercalate entryPathSeparator outPaths

    if stats then do
        putStrLn $ "input size : " ++ (show $ length inString)
        putStrLn $ "output size: " ++ (show $ length outString)
    else do
        putStrLn $ "input   : " ++ show inPaths
        putStrLn $ "decoded : " ++ show absPaths
        putStrLn $ "re-coded: " ++ show outPaths

    case output of
        Nothing -> pure ()
        Just output ->
            putStrLn $ intercalate entryPathSeparator outPaths

entryPathSeparator = ":"

trimnl =
    reverse . dropWhile (=='\n') . reverse
