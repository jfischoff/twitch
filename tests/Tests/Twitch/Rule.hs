{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns  #-}
module Tests.Twitch.Rule where
import Test.Hspec
import Twitch.Rule  
import Filesystem.Path
import Data.IORef
import Data.Default

testSetter sel setter = do
  ref <- newIORef ""
  let rule = def `setter` writeIORef ref
      expected = "yo"
  sel rule expected
  actual <- readIORef ref
  actual `shouldBe` expected
  
testAddModify f = do
  ref <- newIORef ""
  
  let rule = def `f` writeIORef ref
      expected = "yo"
  
  add rule expected
  actual <- readIORef ref  
  actual `shouldBe` expected

  modify rule expected
  actual <- readIORef ref  
  actual `shouldBe` expected

testName :: (Rule -> String -> Rule) -> Expectation
testName f = name (f def "name") `shouldBe` "name"


tests :: Spec
tests = do
  describe "|+" $ it " sets the add member"    $ testSetter add (|+)
  describe "|-" $ it " sets the delete member" $ testSetter delete (|-)
  describe "|%" $ it " sets the modify member" $ testSetter modify (|%)
  
  describe "|>" $ it " sets the add and modify member" $ testAddModify (|>)
  describe "|#" $ it " sets the name member" $ testName (|#)

  describe "addF"       $ it " sets the add member"    $ testSetter add $ flip addF
  describe "modifyF"    $ it " sets the modify member" $ testSetter modify $ flip modifyF
  describe "deleteF"    $ it " sets the delete member" $ testSetter delete $ flip deleteF
  describe "addModifyF" $ it " sets the add and modify member" $ 
    testAddModify (flip addModifyF)
  describe "nameF" $ it " sets the name member" $ testName $ flip nameF
  
  describe "makeAbosolutePath" $ do
    it "doesn't change an absolute path" $ 
      makeAbsolutePath "/usr" "/home" `shouldBe` "/home"
    it "adds the current directory if it is relative" $ do
      makeAbsolutePath "/usr" "./home" `shouldBe` "/usr/./home"
      makeAbsolutePath "/usr"   "home" `shouldBe` "/usr/home"
  
  describe "compilePattern" $ do
    it "'s happy path works" $ do
      let Right test = compilePattern "/home/*.js" 
      test "/home/help.js" `shouldBe` True
      test "/usr/help.js"  `shouldBe` False

    it "with an invalid pattern fails" $ do
      let Left (PatternCompliationFailed !_ !_) = compilePattern "/home/****.js" 
      return () :: IO ()