{-# LANGUAGE RecordWildCards                 #-}
{-# LANGUAGE LambdaCase                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE FlexibleInstances               #-}
module Twitch.Internal where
import Data.Map (Map)
import Control.Applicative
import Control.Monad
import Filesystem.Path
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)
import Data.List 
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.State as State
import System.Directory
import System.FilePath.Glob
import qualified Twitch.Rule as Rule
import Twitch.Rule (Rule)
import Data.Monoid
import Data.Time.Clock
import Data.String
import Data.Default
import Control.Arrow
import Data.Either
import System.FSNotify (WatchManager)

newtype DepM a = DepM { unDepM :: State [Rule] a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState [Rule]
           )

runDep :: Dep -> [Rule]
runDep = runDepWithState mempty

runDepWithState :: [Rule] -> Dep -> [Rule] 
runDepWithState xs = flip execState xs . unDepM 

type Dep = DepM ()

instance IsString Dep where
  fromString = addRule . fromString  
  
addRule r = State.modify (r :)

-- TODO this should probably issue a warning 
modHeadRule :: Dep -> (Rule -> Rule) -> Dep
modHeadRule dep f = do 
  let res = runDep dep
  case res of
    x:xs -> put $ f x : xs
    r    -> put r

infixl 8 |+, |%, |-, |>, |#
(|+), (|%), (|-), (|>) :: Dep -> (FilePath -> IO a) -> Dep
-- | Set the 'add' field
x |+ f = modHeadRule x $ Rule.add' f
-- | Set the modify field
x |% f = modHeadRule x $ Rule.modify' f
-- | Set the delete field
x |- f = modHeadRule x $ Rule.delete' f
-- | Set both the 'add' and 'modify' field to the same value
x |> f = x |+ f |% f

-- | Set the name
(|#) :: Dep -> Text -> Dep
r |# p = modHeadRule r $ Rule.name' p

-- Prefix API -----------------------------------------------------------------
add', modify', delete', addModify :: (FilePath -> IO a) -> Dep -> Dep
add'      = flip (|+)
modify'   = flip (|%)
delete'   = flip (|-)
addModify = flip (|>)