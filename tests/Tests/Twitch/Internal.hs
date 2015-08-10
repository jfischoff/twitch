{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Tests.Twitch.Internal where
import Control.Monad ( forM_ )
import Test.Hspec
import Twitch.Internal ( modHeadRule, runDep )
import Twitch.Rule ( Rule (pattern) )

tests :: Spec
tests = do
  describe "modHeadRule" $ it "should thread state correctly" $ do
    -- `patterns` is used as `[Rule]` and `[Dep]` here
    let patterns = ["*.md", "*.html", "*.hs", "*.cpp", "*.js"]
        dep      = forM_ patterns (flip modHeadRule id)
    fmap pattern (runDep dep) `shouldBe` fmap pattern (reverse patterns)
