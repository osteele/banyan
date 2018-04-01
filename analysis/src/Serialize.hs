module Serialize
    ( decodePaths
    , encodePaths
    , encodePathsRel
    ) where

import Control.Monad.State
import Data.List
import FilePathExtras
import System.FilePath.Posix

type WorkingDirectory = FilePath

type CwdState = State WorkingDirectory

mapPaths :: (FilePath -> CwdState FilePath) -> [FilePath] -> [FilePath]
mapPaths f =  flip evalState "/" . sequence . map f

getCwd :: CwdState WorkingDirectory
getCwd = get

putCwd :: WorkingDirectory -> CwdState ()
putCwd = put

decodePaths :: [FilePath] -> [FilePath]
decodePaths = mapPaths $ \p -> do
    cwd <- getCwd
    let p' = if isRelative p then joinPath [cwd, p] else p
    when (isDirectory p') $ putCwd p'
    return p'

encodePaths :: [FilePath] -> [FilePath]
encodePaths = mapPaths $ \p -> do
    cwd <- getCwd
    let
        p' =
            case stripPrefix cwd p of
                Just rel -> rel
                Nothing -> p
    when (isDirectory p) $ putCwd p
    return p'

encodePathsRel :: [FilePath] -> [FilePath]
encodePathsRel = mapPaths $ \p -> do
    cwd <- getCwd
    let p' = makeRelativeWithDots cwd p
    when (isDirectory p) $ putCwd p
    return p'
