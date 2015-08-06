{-# LANGUAGE OverloadedStrings #-}
module Tests.Twitch.Internal where
import Test.Hspec
import Twitch.Internal ( modHeadRule, runDep )

tests :: Spec
tests = do
  describe "modHeadRule" $ it "should thread state correctly" $ do
    let patterns  = ["*.md", "*.html", "*.hs", "*.cpp", "*.js"]
        deps      = fmap (flip modHeadRule id) patterns
    (length . runDep $ sequence_ deps) `shouldBe` length patterns
