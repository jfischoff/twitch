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

-- | A polymorphic 'Dep'. Exported for completeness, ignore. 
newtype DepM a = DepM { unDepM :: State [Rule] a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState [Rule]
           )

-- | This is the key type of the package, it is where rules are accumulated.
type Dep = DepM ()

instance IsString Dep where
  fromString = addRule . fromString

runDep :: Dep -> [Rule]
runDep = runDepWithState mempty

runDepWithState :: [Rule] -> Dep -> [Rule] 
runDepWithState xs = flip execState xs . unDepM 

addRule r = State.modify (r :)

modHeadRule :: Dep -> (Rule -> Rule) -> Dep
modHeadRule dep f = do 
  let res = runDep dep
  case res of
    x:xs -> put $ f x : xs
    r    -> put r

infixl 8 |+, |%, |-, |>, |#
(|+), (|%), (|-), (|>) :: Dep -> (FilePath -> IO a) -> Dep
-- | Add a \'add\' callback
x |+ f = modHeadRule x $ Rule.addF f
-- | Add a \'modify\' callback
x |% f = modHeadRule x $ Rule.modifyF f
-- | Add a \'delete' callback
x |- f = modHeadRule x $ Rule.deleteF f
-- | Add the same callback for the \'add\' and the \'modify\' events.
x |> f = x |+ f |% f

-- | Set the name of a rule. Useful for debugging when logging is enabled.
--   Rules names default to the glob pattern.
(|#) :: Dep -> Text -> Dep
r |# p = modHeadRule r $ Rule.nameF p

-- Prefix API -----------------------------------------------------------------
add, modify, delete, addModify :: (FilePath -> IO a) -> Dep -> Dep
-- | Add a \'add\' callback
add      = flip (|+)
-- | Add a \'modify\' callback
modify   = flip (|%)
-- | Add a \'delete' callback
delete   = flip (|-)
-- | Add the same callback for the \'add\' and the \'modify\' events.
addModify = flip (|>)