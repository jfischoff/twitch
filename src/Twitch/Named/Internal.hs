{-# LANGUAGE RecordWildCards #-}
module Pish.Safe where

-- The idea is that there is a typeclass for producing rules
-- the generic instance uses the selector names

-- The goal is to write something like

test = def {..} where
  selector1 = "this.txt" |> [s|echo $baseName.png|] -- Make this a quasiquoted thing
  selector2 = "that.txt" { add    = [s|echo $dirName/blah.$ext|]
                         , modify = \path -> print path 
                         }
gen :: ToRules a => a -> IO WatchManager

