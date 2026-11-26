{-# LANGUAGE DataKinds, NoDependentTypes, PolyKinds, StandaloneKindSignatures, TypeApplications, TypeFamilies #-}
module NativeOpenMatchabilityDistinctIndices where
import Data.Kind (Type)
import GHC.Exts (Matchability)
type K :: forall (m :: Matchability). Bool -> Type
type family K b
type Consume :: forall (m :: Matchability). K @m True -> Type
type family Consume p
type Reject :: forall (m :: Matchability) (n :: Matchability).
               forall (p :: K @m True) -> Consume @n p
type family Reject p
