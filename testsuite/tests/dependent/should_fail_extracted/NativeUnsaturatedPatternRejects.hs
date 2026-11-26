{-# LANGUAGE DataKinds, DependentTypes, ExplicitForAll, FlexibleInstances, ListTuplePuns #-}
{-# LANGUAGE Modifiers, PolyKinds, TypeFamilies, TypeOperators #-}
module NativeUnsaturatedPatternRejects where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.Matchability (M, Matchability, U)

class C t
instance forall (f :: Type %U -> Type) x. C (f x)

type family ToU (m :: Matchability) where
  ToU M = U

class Computed t
instance forall (f :: Type %(ToU M) -> Type) x. Computed (f x)

type family UnApp p q r where
  forall (f :: Type %U -> Type) x. UnApp (Proxy f) (Proxy x) (f x) = x

class CRigid t
instance forall (m :: Matchability) (f :: Type %m -> Type) x. (m ~ M) => CRigid (f x)
