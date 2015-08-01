{-# LANGUAGE OverloadedStrings #-}
module Tests.Twitch.Run where
import Test.Hspec
import System.FilePath ( FilePath )
import Control.Arrow (second)
import qualified Twitch.InternalRule as IR
import Twitch.Run
import Twitch
-- test depToRules

tests :: Spec
tests = describe "Dep" $ do
  it "depToRules happy path" $ 
    shouldBe (second (map IR.name) (depToRules "/usr/" ("*.js" |+ print)))  
             ([], ["*.js"])
