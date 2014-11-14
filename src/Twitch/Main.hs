module Main where

-- Different File
-- Main
type Issue = String  

class Eval alg config | alg -> config where
  eval :: config -> ([Issue], Either Issue [Rule])

-- Just use a list for testing
instance Eval [Rule] Config where
  eval = evalRules 

errorLeft :: Either String a -> a
errorLeft = either error id

defaultMain :: (Eval alg config, Parse config) => alg -> IO ()
defaultMain alg = do
  config <- errorLeft <$> parseCmdLine    
  defaultWithConfig config alg

defaultWithConfig :: (Eval alg config, HasSilent config) => config -> alg -> IO ()
defaultWithConfig config alg = do
  (issues, rules) <- second errorLeft <$> eval config alg
  when (not $ null issues) $ putStrLn $ unlines issues
  seq rules $ evalRules config rules

  unless (silent config) $ putStrLn "pish-posh is running. Hit any key to stop it."
  _ <- getLine
  stopManager man
  return ()