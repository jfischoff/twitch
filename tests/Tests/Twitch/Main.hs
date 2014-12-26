{-# LANGUAGE OverloadedStrings #-}
module Tests.Twitch.Main where
import Test.Hspec
import Test.QuickCheck
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
    
    it "parses the directories option" $ do
      testParser dirsToWatch "--directory=./src" ["./src"]
      testParser dirsToWatch "-d./src"             ["./src"]
      testParserMulti dirsToWatch ["--directory=./src", "--directory=./temp"] 
                                  ["./src", "./temp"]
      testParserMulti dirsToWatch ["-d./src", "-d./temp"] ["./src", "./temp"]
      
    
    it "parses the recurse option" $ do
      testParser recurseThroughDirectories "--no-recurse" False
      testParser recurseThroughDirectories "-r"  False

    it "parses the debounce option" $ do
      testParser debounce "--debounce=DebounceDefault" DebounceDefault
      testParser debounce "--debounce=Debounce"        Debounce
      testParser debounce "--debounce=NoDebounce"      NoDebounce
    
      testParser debounce "-bDebounceDefault" DebounceDefault
      testParser debounce "-bDebounce"        Debounce
      testParser debounce "-bNoDebounce"      NoDebounce
    
    it "parses the debounce-amount option" $ do
      testParser debounceAmount "--debounce-amount=1.0" 1.0
      testParser debounceAmount "-a1.0" 1.0
    
    it "parses the poll-interval" $ do
      testParser pollInterval "--poll-interval=1" 1
      testParser pollInterval "-i1" 1
      
    it "parses the poll" $ do
      testParser usePolling "--should-poll" True      
      testParser usePolling "-p"     True

    it "parses the current directory" $ do
      testParser currentDir "--current-dir=./src" $ Just "./src"
      testParser currentDir "-c./src"             $ Just "./src"
      
      
    