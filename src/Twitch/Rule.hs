{-# LANGUAGE RecordWildCards                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE FlexibleInstances               #-}
module Twitch.Rule where
import Prelude hiding (FilePath)
import Control.Monad ( void )
import Filesystem.Path ( FilePath )
import Filesystem.Path.CurrentOS ( encodeString )
import System.FilePath.Glob ( simplify, match, tryCompileWith, compDefault )
import Data.Monoid ( (<>) )
import Data.String ( IsString(..) )
import Data.Default ( Default(..) )
import Control.Arrow ( ArrowChoice(left) )


type Name        = String
type PatternText = String

-- | TODO maybe change this to have the timestamp
type RuleAction = FilePath -> IO ()

-- | The pattern entity holds a name and pattern that is compiled 
-- when the rules are evaluated

-- It is worth noting that the entire API could just be this
-- Record with Default
-- There are actually three apis
-- "foo.x" .# "name" .$ \x -> print x
-- "foo.x" |> \x -> print x
-- "foo.txt" 
--    { add    = \x -> print x
--    , modify = \x -> print x
--    }

data Rule = Rule 
  { name          :: String
  , pattern       :: PatternText
  , add           :: RuleAction
  , modify        :: RuleAction
  , delete        :: RuleAction
  }

instance Default Rule where
  def = Rule 
          { name    = ""
          , pattern = ""
          , add     = def
          , modify  = def
          , delete  = def
          }

instance IsString Rule where
  fromString x = def { pattern = x, name = x} 

--- Infix API------------------------------------------------------------------
infixl 8 |+, |%, |-, |>, |#
(|+), (|%), (|-), (|>) :: Rule -> (FilePath -> IO a) -> Rule
-- | Set the 'add' field
--   ex.
-- 
--   > "doodle.md |+ ringBell "
x |+ f = x { add = void . f }
-- | Set the modify field
x |% f = x { modify = void . f }
-- | Set the delete field
x |- f = x { delete = void . f }
-- | Set both the 'add' and 'modify' field to the same value
x |> f = x |+ f |% f

-- | Set the name
(|#) :: Rule -> String -> Rule
r |# p = r { name = p }

-- Prefix API -----------------------------------------------------------------
addF, modifyF, deleteF, addModifyF :: (FilePath -> IO a) -> Rule -> Rule
addF       = flip (|+)
modifyF    = flip (|%)
deleteF    = flip (|-)
addModifyF = flip (|>)

nameF :: String -> Rule -> Rule
nameF = flip (|#)

-- def & add foo & modify foo & delete foo & test tester
-- def & add foo & modify foo & delete foo & pattern tester

data RuleIssue
  = PatternCompliationFailed PatternText String
  deriving Show

compilePattern :: FilePath 
               -> PatternText 
               -> Either RuleIssue (FilePath -> Bool)
compilePattern currentDir pattern = left (PatternCompliationFailed pattern) $ do 
   -- TODO is this way of adding the current directory cross platfrom okay?
   -- Does the globbing even work on windows
   
   let absolutePattern = encodeString currentDir <> "/" <> pattern
   p <- tryCompileWith compDefault absolutePattern
   let test = match $ simplify p
   return $ \x -> test $ encodeString x