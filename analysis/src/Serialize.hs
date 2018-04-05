module Serialize
    ( decodePaths
    , encodePaths
    , encodePathsMultidot
    , encodePathsRel
    ) where

import Control.Monad.State
import Data.List
import System.FilePath.Posix

import FilePathExtras

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

mkEncoder :: (FilePath -> FilePath -> FilePath) -> [FilePath] -> [FilePath]
mkEncoder enc = mapPaths $ \p -> do
    cwd <- getCwd
    let p' = enc cwd p
    when (isDirectory p) $ putCwd p
    return p'

encodePaths :: [FilePath] -> [FilePath]
encodePaths = mkEncoder $ \cwd p ->
    case stripPrefix cwd p of
        Just rel -> rel
        Nothing -> p

encodePathsRel :: [FilePath] -> [FilePath]
encodePathsRel = mkEncoder makeShortestRelative

encodePathsMultidot :: [FilePath] -> [FilePath]
encodePathsMultidot = mkEncoder makeShortestMultidots
