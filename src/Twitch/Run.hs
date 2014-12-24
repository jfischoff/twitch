module Twitch.Run where
import Prelude hiding (FilePath, log)
import Twitch.Internal ( Dep, runDep )
import Twitch.InternalRule
    ( Config(dirs, logger), InternalRule, toInternalRule, setupRules )
import Twitch.Rule ( RuleIssue )
import Data.Either ( partitionEithers )
import Filesystem.Path ( FilePath )
import Filesystem.Path.CurrentOS ( decodeString )
import Control.Applicative ( (<$>) )
import System.FSNotify ( WatchManager )
import System.Directory ( getCurrentDirectory )
import Twitch.Path ( findAllDirs )
import Data.Default ( Default(def) )

-- This the main interface for running a Dep

run :: Dep -> IO WatchManager
run dep =  do
  currentDir <- decodeString <$> getCurrentDirectory
  dirs'       <- findAllDirs currentDir
  runWithConfig currentDir (def { logger = print, dirs = dirs' }) dep

runWithConfig :: FilePath -> Config -> Dep -> IO WatchManager
runWithConfig currentDir config dep = do
  let (_issues, rules) = depToRules currentDir dep
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


  



  