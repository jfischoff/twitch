{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Logger where
import Control.Exception
import Data.Typeable


data FooArgument = FooArgument Int
  deriving (Show, Typeable)

instance Exception FooArgument

withLogger :: (Exception e, Exception e2) => (e -> IO ()) -> e2 -> IO () 
withLogger log = maybe (return ()) log . fromException . toException

foo :: Exception e => (e -> IO ()) -> Int -> IO Int
foo logger x = do
  withLogger logger $ FooArgument x
  return $ x + 1

