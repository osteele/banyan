module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Semigroup ((<>))
import Options.Applicative

import FilePathExtras (compareByDirectory)
import Serialize

data Options = Options
  { _stats :: Bool
  , _sort :: Bool
  , _useDots :: Bool
  , _combineDots :: Bool
  , _output :: Maybe String
  , _input :: String
  }

options :: Parser Options
options =
  Options <$> switch (long "stats" <> short 's' <> help "Display statistics") <*>
  switch (long "sort" <> help "Sort files before directories") <*>
  switch (long "use-dots" <> short 'd' <> help "Allow .. in relative pathnames") <*>
  switch
    (long "combine-dots" <> short 'm' <> help "Allow ... etc. in pathnames") <*>
  optional
    (strOption
       (long "output" <> short 'o' <> metavar "FILE" <>
        help "Write output to FILE")) <*>
  argument str (metavar "FILE")

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        (fullDesc <> progDesc "Report path cache metrics" <>
         header "analyze - report path cache metrics")

run :: Options -> IO ()
run opts@(Options stats _ dots multidots output infile) = do
  inString <- trimnl <$> readFile infile
  let encoder =
        case (dots, multidots) of
          (False, _) -> encodePaths
          (True, False) -> encodePathsRel
          (True, True) -> encodePathsMultidot
      sorter =
        if _sort opts
          then sortBy compareByDirectory
          else id
  let inPaths = splitOn entryPathSeparator inString
      absPaths = decodePaths inPaths
      sortedPaths = sorter absPaths
      outPaths = encoder sortedPaths
      outString = intercalate entryPathSeparator outPaths
  if stats
    then do
      putStrLn $ "input size : " ++ show (length inString)
      putStrLn $ "output size: " ++ show (length outString)
    else do
      putStrLn $ "input   : " ++ show inPaths
      putStrLn $ "decoded : " ++ show absPaths
      putStrLn $ "re-coded: " ++ show outPaths
  case output of
    Nothing -> pure ()
    Just _ -> putStrLn $ intercalate entryPathSeparator outPaths

entryPathSeparator :: String
entryPathSeparator = ":"

trimnl :: String -> String
trimnl = reverse . dropWhile (== '\n') . reverse
