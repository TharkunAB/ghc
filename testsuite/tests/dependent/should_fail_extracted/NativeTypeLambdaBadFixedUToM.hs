{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, Modifiers, PolyKinds, RankNTypes #-}
module NativeTypeLambdaBadFixedUToM where
import Data.Kind (Type)
import GHC.Exts (U)
badFixedUToM :: (forall (f :: Type %U -> Type). f Bool) -> Maybe Bool
badFixedUToM x = x
