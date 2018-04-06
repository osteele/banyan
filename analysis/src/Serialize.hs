module Serialize
  ( decodePaths
  , encodePaths
  , encodePathsMultidot
  , encodePathsRel
  ) where

import           Control.Monad.State
import           Data.Function         ((&))
import           Data.List             (stripPrefix)
import           Data.Maybe            (fromMaybe)
import           FilePathExtras
import           System.FilePath.Posix

type WorkingDirectory = FilePath

type CwdState = State WorkingDirectory

mapPaths :: (FilePath -> CwdState FilePath) -> [FilePath] -> [FilePath]
mapPaths f = flip evalState "/" . mapM f

getCwd :: CwdState WorkingDirectory
getCwd = get

putCwd :: WorkingDirectory -> CwdState ()
putCwd = put

decodePaths
  :: [FilePath] -- relative paths
  -> [FilePath] -- absolute paths
decodePaths =
  mapPaths $ \p -> do
    cwd <- getCwd
    let p' =
          if isRelative p
            then joinPath [cwd, p]
            else p
    when (isDirectory p') $ putCwd p'
    return p'

mkEncoder :: (FilePath -> FilePath -> FilePath) -> [FilePath] -> [FilePath]
mkEncoder enc =
  mapPaths $ \p -> do
    cwd <- getCwd
    let p' = enc cwd p
    when (isDirectory p) $ putCwd p
    return p'

encodePaths :: [FilePath] -> [FilePath]
encodePaths = mkEncoder $ \base path -> stripPrefix base path & fromMaybe path

encodePathsRel :: [FilePath] -> [FilePath]
encodePathsRel = mkEncoder makeShortestRelative

encodePathsMultidot :: [FilePath] -> [FilePath]
encodePathsMultidot = mkEncoder makeShortestMultidots
