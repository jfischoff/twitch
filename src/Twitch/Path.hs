--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Twitch.Path
       ( findFiles
       , findDirs
       , findAllDirs
       , canonicalizeDirPath
       , canonicalizePath
       ) where
import Control.Applicative -- satisfy GHC < 7.10
import Control.Monad
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , getDirectoryContents
  )
import System.FilePath
  ( FilePath
  , (</>)
  , addTrailingPathSeparator
  )
-- Moved here to suppress redundant import warnings for GHC > 7.10
import Prelude hiding (FilePath)


getDirectoryContentsPath :: FilePath -> IO [FilePath]
getDirectoryContentsPath path =
  map (path </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents path

fileDirContents :: FilePath -> IO ([FilePath],[FilePath])
fileDirContents path = do
  contents <- getDirectoryContentsPath path
  files <- filterM doesFileExist contents
  dirs <- filterM doesDirectoryExist contents
  return (files, dirs)

findAllFiles :: FilePath -> IO [FilePath]
findAllFiles path = do
  (files, dirs) <- fileDirContents path
  nestedFiles <- mapM findAllFiles dirs
  return (files ++ concat nestedFiles)

findImmediateFiles, findImmediateDirs :: FilePath -> IO [FilePath]
findImmediateFiles = getDirectoryContentsPath >=> filterM doesFileExist >=> canonicalize
  where
    canonicalize :: [FilePath] -> IO [FilePath]
    canonicalize files = mapM canonicalizePath files
findImmediateDirs  = getDirectoryContentsPath >=> filterM doesDirectoryExist >=> canonicalize
  where
    canonicalize :: [FilePath] -> IO [FilePath]
    canonicalize dirs = mapM canonicalizeDirPath dirs

findAllDirs :: FilePath -> IO [FilePath]
findAllDirs path = do
  dirs <- findImmediateDirs path
  nestedDirs <- mapM findAllDirs dirs
  return (dirs ++ concat nestedDirs)

findFiles :: Bool -> FilePath -> IO [FilePath]
findFiles True path  = findAllFiles       =<< canonicalizeDirPath path
findFiles False path = findImmediateFiles =<< canonicalizeDirPath path

findDirs :: Bool -> FilePath -> IO [FilePath]
findDirs True path  = findAllDirs       =<< canonicalizeDirPath path
findDirs False path = findImmediateDirs =<< canonicalizeDirPath path


canonicalizeDirPath :: FilePath -> IO FilePath
canonicalizeDirPath path = addTrailingPathSeparator <$> canonicalizePath path

