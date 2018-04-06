module Serialize
  ( decodePaths
  , mkEncoder
  ) where

import           Control.Monad.State
-- import           Data.Function         ((&))
-- import           Data.List             (stripPrefix)
-- import           Data.Maybe            (fromMaybe)
import           FilePathExtras
import           System.FilePath.Posix

type AbsolutePath = FilePath
type RelativePath = FilePath
type WorkingDirectory = AbsolutePath
type CwdState = State WorkingDirectory

getCwd :: CwdState WorkingDirectory
getCwd = get

putCwd :: WorkingDirectory -> CwdState ()
putCwd = put

mapPaths :: (FilePath -> CwdState FilePath) -> [FilePath] -> [FilePath]
mapPaths f = flip evalState "/" . mapM f

decodePaths :: [RelativePath] -> [AbsolutePath]
decodePaths =
  mapPaths $ \p -> do
    cwd <- getCwd
    let p' =
          if isRelative p
            then joinPath [cwd, p]
            else p
    when (isDirectory p') $ putCwd p'
    return p'

mkEncoder :: Relativizer -> [AbsolutePath] -> [RelativePath]
mkEncoder enc =
  mapPaths $ \p -> do
    cwd <- getCwd
    let p' = enc cwd p
    when (isDirectory p) $ putCwd p
    return p'
