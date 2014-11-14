{-# LANGUAGE RecordWildCards                 #-}
{-# LANGUAGE LambdaCase                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE FlexibleInstances               #-}
module Twitch.Rule where
import Data.Map (Map)
import Control.Applicative
import Control.Monad
import Filesystem.Path
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath.Glob
import Data.Monoid
import Data.Time.Clock
import Data.String
import Data.Default
import Control.Arrow
import Data.Either
import Debug.Trace
import System.FSNotify (WatchManager)

-- It doesn't appear that the current directory is need in the monad
-- The new way I am thinking about this
-- This is a Rule type and what is currently called Rule is called 
-- InternalRule

type Name        = Text
type PatternText = Text

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
  { name          :: Text
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
  fromString x = let packed = T.pack x in def { pattern = packed, name = packed} 

--- Infix API------------------------------------------------------------------
infixl 8 |+, |%, |-, |>, |#
(|+), (|%), (|-), (|>) :: Rule -> (FilePath -> IO a) -> Rule
-- | Set the 'add' field
x |+ f = x { add = void . f }
-- | Set the modify field
x |% f = x { modify = void . f }
-- | Set the delete field
x |- f = x { delete = void . f }
-- | Set both the 'add' and 'modify' field to the same value
x |> f = x |+ f |% f

-- | Set the name
(|#) :: Rule -> Text -> Rule
r |# p = r { name = p }

-- Prefix API -----------------------------------------------------------------
add', modify', delete', addModify :: (FilePath -> IO a) -> Rule -> Rule
add'      = flip (|+)
modify'   = flip (|%)
delete'   = flip (|-)
name'     = flip (|#)
addModify = flip (|>)

-- def & add foo & modify foo & delete foo & test tester
-- def & add foo & modify foo & delete foo & pattern tester

data RuleIssue
  = PatternCompliationFailed PatternText String
  deriving Show

compilePattern :: FilePath -> PatternText -> Either RuleIssue (FilePath -> Bool)
compilePattern currentDir pattern = left (PatternCompliationFailed pattern) $ do 
   -- TODO is this way of adding the current directory cross platfrom okay?
   -- Does the globbing even work on windows
   
   let absolutePattern = traceShowId $ encodeString currentDir <> "/" <> T.unpack pattern
   p <- tryCompileWith compDefault absolutePattern
   let test = match $ simplify p
   return $ \x -> test $ encodeString x