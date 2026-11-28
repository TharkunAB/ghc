{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeOperators #-}
module NativeIndexedForEachBadProof where
import Data.Type.Equality ((:~:)(Refl))
import NativeIndexedForEachCtorProvider
badProof :: foreach (t :: UTerm 'Zero) -> 'ULam ('UVar 'FZ) :~: t
badProof _ = Refl
