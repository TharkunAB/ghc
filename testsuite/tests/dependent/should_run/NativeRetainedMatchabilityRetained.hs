{-# LANGUAGE BangPatterns, DependentTypes, ListTuplePuns #-}
module NativeRetainedMatchabilityRetained where
import GHC.Exts (Matchability, U)

data Reading (tag :: Bool) (m :: Maybe a) = Reading Int

forward :: forall (ma :: Matchability). ma ~ U =>
           foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Reading tag m %ma -> Reading tag m
forward _ (type a) _ x = x

{-# NOINLINE strictPrefix #-}
strictPrefix :: forall (ma :: Matchability). ma ~ U =>
                foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Reading tag m -> Int -> Int %ma -> Int
strictPrefix _ (type a) _ _ !_ x = x

{-# NOINLINE constructor #-}
constructor :: forall (ma :: Matchability). ma ~ U =>
               foreach (tag :: Bool) -> Eq Int => foreach (m :: Maybe Bool) -> Reading tag m -> Int %ma -> Int
constructor False _ _ x = x
constructor True _ _ x = x
