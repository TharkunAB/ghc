{-# LANGUAGE DependentTypes #-}
module NativeLocalOpenTermCycle where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenTermCycle.choose a
choose x = loop x
loop x = loop x
