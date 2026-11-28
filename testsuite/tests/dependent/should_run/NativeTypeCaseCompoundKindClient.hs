{-# LANGUAGE DataKinds, NoDependentTypes, PolyKinds, RankNTypes, TypeApplications #-}
module NativeTypeCaseCompoundKindClient where
import Data.Proxy (Proxy(..))
import NativeTypeCaseFieldCompoundKind

outer :: forall k l (a :: Tag k l). Compound k l a -> Proxy a
outer (Compound f) = f @Int @Double @'True Proxy

inner :: forall k l (a :: Tag k l). Compound k l a -> Proxy '(Int, Double)
inner (Compound f) = f @Int @Double @'False Proxy
