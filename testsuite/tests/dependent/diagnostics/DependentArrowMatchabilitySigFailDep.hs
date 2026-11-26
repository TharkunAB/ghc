{-# LANGUAGE DependentTypes, TypeFamilies, UndecidableInstances #-}
module DependentArrowMatchabilitySigFailDep where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import qualified DependentArrowProvider as P
import GHC.Exts (FUN, M, U, Matchability, Multiplicity(Many))

-- | Keep the written modifier even when a later use needs U.
laterBinderBad :: forall (f :: Type %M -> Type). Proxy (P.D (Type %U -> Type) f) -> Proxy f -> Proxy f
laterBinderBad _ = id

-- | Reject a binder kind that depends on a variable bound later in the telescope.
scopedBinderBad :: forall (f :: Type -> Type) (m :: Matchability).
                   Proxy (P.D (FUN @m 'Many Type Type) f) -> Proxy f -> Proxy f
scopedBinderBad _ = id

-- | Reject a written inner M arrow applied to an unmatchable family.
nestedBinderBad :: forall (g :: (Type %M -> Type) -> Type).
                   Proxy (g P.Abstract) -> Proxy g -> Proxy g
nestedBinderBad _ = id
