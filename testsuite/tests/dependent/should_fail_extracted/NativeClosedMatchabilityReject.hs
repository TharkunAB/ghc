{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, PolyKinds, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeClosedMatchabilityReject where
import Data.Kind (Type)
import GHC.Exts (FUN, M, U, Multiplicity(Many))
import NativeClosedMatchabilityProvider (FixedNullary, K)

type family Interpret (b :: Bool) :: FixedNullary
type instance Interpret 'True = Maybe

data Bridge (k :: Type) (f :: k) (g :: k)
type Wrong (f :: FUN @M 'Many Type Type) (g :: FUN @U 'Many Type Type)
  = Bridge (K 'True) f g
