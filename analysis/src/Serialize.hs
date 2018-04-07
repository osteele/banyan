{-# LANGUAGE NoImplicitPrelude #-}

module Serialize
  ( decodeFilePaths
  , fileSizes
  , mkFilePathsEncoder
  ) where

import qualified Prelude               as UnsafePartial (read)
import           Protolude
import           System.FilePath.Posix

import           FilePathExtras

type AbsolutePath = FilePath
type RelativePath = FilePath

-- | A Monad whose state is the current working directory.
type CwdState = State AbsolutePath

getCwd :: CwdState AbsolutePath
getCwd = get

putCwd :: AbsolutePath -> CwdState ()
putCwd = put

mapFilePaths :: (FilePath -> CwdState FilePath) -> [FilePath] -> [FilePath]
mapFilePaths f = flip evalState "/" . mapM f

decodeFilePaths :: [RelativePath] -> [AbsolutePath]
decodeFilePaths =
  mapFilePaths $ \p -> do
    cwd <- getCwd
    let p' =
          if isRelative p
            then joinPath [cwd, p]
            else p
    when (isDirectory p') $ putCwd p'
    return p'

mkFilePathsEncoder :: Relativizer -> [AbsolutePath] -> [RelativePath]
mkFilePathsEncoder enc =
  mapFilePaths $ \p -> do
    cwd <- getCwd
    let p' = enc cwd p
    when (isDirectory p) $ putCwd p
    return p'

fileSizes :: [FilePath] -> [(FilePath, Int)]
fileSizes = fmap parse . filter (not . isDirectory)
    where parse path =
            case break (== ';')  path of
              (prefix, ';' : size) -> (prefix, UnsafePartial.read size)
              _                    -> (path, 0)
