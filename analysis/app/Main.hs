module Main where

import           Data.List
import           Data.List.Split     (splitOn)
import           Data.Semigroup      ((<>))
import           Options.Applicative

import           FilePathExtras      (compareByDirectory)
import           Serialize

data Options = Options
  { getDisplayStats  :: Bool
  , getSortFlag      :: Bool
  , getDotsFlag      :: Bool
  , getMultidotsFlag :: Bool
  , getOutputFile    :: Maybe String
  , getInputFile     :: String
  }

options :: Parser Options
options =
  Options
  <$> switch (long "stats" <> short 's' <> help "Display statistics")
  <*> switch (long "sort" <> help "Sort files before directories")
  <*> switch (long "use-dots" <> short 'd' <> help "Allow .. in relative pathnames")
  <*> switch
    (long "combine-dots" <> short 'm' <> help "Allow ... etc. in pathnames")
  <*> optional (strOption
        (long "output" <> short 'o' <> metavar "FILE" <>
        help "Write output to FILE"))
        <*> argument str (metavar "FILE")

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
        (fullDesc <> progDesc "Report path cache metrics" <>
         header "analyze - report path cache metrics")

run :: Options -> IO ()
run opts = do
  content <- trimnl <$> readFile (getInputFile opts)
  let encoder =
        case (getDotsFlag opts, getMultidotsFlag opts) of
          (False, _)    -> encodePaths
          (True, False) -> encodePathsWithDots
          (True, True)  -> encodePathsWithMultidots
      sorter =
        if getSortFlag opts
          then sortBy compareByDirectory
          else id
  let inPaths     = entryPaths content
      absPaths    = decodePaths inPaths
      sortedPaths = sorter absPaths
      outPaths    = encoder sortedPaths
      outString   = unEntryPaths outPaths
  if getDisplayStats opts
    then do
      putStrLn $ "input size : " ++ show (length content)
      putStrLn $ "output size: " ++ show (length outString)
    else do
      putStrLn $ "input   : " ++ show inPaths
      putStrLn $ "decoded : " ++ show absPaths
      putStrLn $ "re-coded: " ++ show outPaths
  case getOutputFile opts of
    Nothing      -> return ()
    Just outfile -> writeFile outfile outString

-- remove a final newline if present
trimnl :: String -> String
trimnl = reverse . dropWhile (== '\n') . reverse

entryPathSeparator :: String
entryPathSeparator = ":"

entryPaths :: String -> [FilePath]
entryPaths = splitOn entryPathSeparator

unEntryPaths :: [FilePath] -> String
unEntryPaths = intercalate entryPathSeparator
