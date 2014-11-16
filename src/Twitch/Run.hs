module Twitch.Run where
import Twitch.Internal
import Twitch.InternalRule
import Twitch.Rule (RuleIssue)
import Data.Either
import Prelude hiding (FilePath, log)
import Filesystem.Path
import Filesystem.Path.CurrentOS
import Control.Applicative
import System.FSNotify
import System.Directory
import Twitch.Path
import Data.Default
import Debug.Trace

-- This the main interface for running a Dep

run :: Dep -> IO WatchManager
run dep =  do
  currentDir <- decodeString <$> getCurrentDirectory
  dirs       <- findAllDirs currentDir
  runWithConfig currentDir (def { logger = print, dirs = dirs }) dep

runWithConfig :: FilePath -> Config -> Dep -> IO WatchManager
runWithConfig currentDir config dep = do
  let (issues, rules) = depToRules currentDir dep
  -- TODO handle the issues somehow
  -- Log and perhaps error
  setupRules config rules

depToRulesWithCurrentDir :: Dep -> IO ([RuleIssue], [InternalRule])
depToRulesWithCurrentDir dep = do 
  currentDir <- decodeString <$> getCurrentDirectory 
  return $ depToRules currentDir dep

depToRules :: FilePath -> Dep -> ([RuleIssue], [InternalRule])
depToRules currentDir 
  = partitionEithers . map (toInternalRule currentDir) . runDep


  



  