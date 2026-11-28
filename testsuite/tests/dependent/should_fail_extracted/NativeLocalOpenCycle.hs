{-# LANGUAGE DependentTypes #-}
module NativeLocalOpenCycle where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenCycle.keep a
keep :: a -> Result a
keep x = x
