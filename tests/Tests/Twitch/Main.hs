{-# LANGUAGE OverloadedStrings #-}
module Tests.Twitch.Main where
import Test.Hspec
import Options.Applicative.Extra
import Data.Monoid
import Twitch.Main
import Options.Applicative.Builder
import Prelude hiding (log)

runParser :: [String] -> Maybe Options
runParser xs = getParseResult $ execParserPure (prefs mempty) opts xs

-- testParser :: String -> 
testParser f initial expected 
  = fmap f (runParser [initial]) `shouldBe` Just expected
  
testParserMulti f xs expected 
  = fmap f (runParser xs) `shouldBe` Just expected

tests :: Spec
tests = do
  describe "Command Line Parser" $ do
    it "parses the log option" $ do
      -- long
      testParser log "--log=LogToStdout" LogToStdout
      testParser log "--log=LogToFile"   LogToFile
      testParser log "--log=NoLogger"    NoLogger
      -- short
      testParser log "-lLogToStdout" LogToStdout
      testParser log "-lLogToFile"   LogToFile
      testParser log "-lNoLogger"    NoLogger
    
    it "parses the log-file option (long)" $ 
      testParser logFile "--log-file=./log.txt" $ Just "./log.txt" 
    it "parses the log-file option (short)" $ 
      testParser logFile "-f./log.txt"          $ Just "./log.txt"
    
    it "parses the root option" $ do
      testParser root "--root=./src" $ Just "./src"
      testParser root "-r./src"      $ Just "./src"
    
    it "parses the recurse option" $ do
      testParser recurseThroughDirectories "--no-recurse" False
      testParser recurseThroughDirectories "-n"  False
    
    it "parses the poll-interval" $ do
      testParser pollInterval "--poll-interval=1" 1
      testParser pollInterval "-i1" 1
      
    it "parses the poll" $ do
      testParser usePolling "--should-poll" True      
      testParser usePolling "-p"     True
