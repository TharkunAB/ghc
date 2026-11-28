{-# LANGUAGE BangPatterns, DependentTypes #-}
module NativeLocalOpenStrict where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenStrict.keep a
keep :: a -> a
keep !x = x
