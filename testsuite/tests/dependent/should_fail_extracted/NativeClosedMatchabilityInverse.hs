{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, PolyKinds, TypeApplications, TypeOperators #-}
module NativeClosedMatchabilityInverse where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, U, Multiplicity(Many))

badInverse :: forall (f :: FUN @U 'Many Type Type) a b. (f a ~ f b) => a :~: b
badInverse = Refl
