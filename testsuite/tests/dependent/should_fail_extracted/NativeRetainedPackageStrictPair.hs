{-# LANGUAGE BangPatterns, DependentTypes, ListTuplePuns #-}
module NativeRetainedPackageStrictPair (DPair(..), strictFirst) where
import Data.Kind (Type)

data DPair (a :: Type) (p :: a -> Type) where
    Pair :: foreach (x :: a) -> p x -> DPair a p

strictFirst :: DPair a p -> a
strictFirst (Pair x !_) = x
