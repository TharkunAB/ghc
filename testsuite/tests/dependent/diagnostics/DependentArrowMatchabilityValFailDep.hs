{-# LANGUAGE DependentTypes, TypeFamilies, UndecidableInstances #-}
module DependentArrowMatchabilityValFailDep where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import DependentArrowMatchability (ascribedDefault, binderDefault, nestedBinder)
import qualified DependentArrowProvider as P
import GHC.Exts (FUN, U, Multiplicity(Many))

type family NestedAbstract :: (Type %U -> Type) %U -> Type

-- | Reject decomposition at an unmatchable head.
inverseU :: forall (f :: FUN @U 'Many Type Type) a b.
            Proxy f -> f a :~: f b -> a :~: b
inverseU _ Refl = Refl

-- | Reject an unmatchable family passed to an M-defaulted binder.
badDefault :: Proxy P.Abstract -> Proxy P.Abstract
badDefault = ascribedDefault @P.Abstract

-- | Reject an unmatchable family passed to an unconstrained M-defaulted binder.
badBinderDefault :: Proxy P.Abstract -> Proxy P.Abstract
badBinderDefault = binderDefault @P.Abstract

-- | Reject an outer U family passed to an outer M-defaulted binder.
badNestedDefault :: Proxy (NestedAbstract P.Abstract) -> Proxy NestedAbstract -> Proxy NestedAbstract
badNestedDefault = nestedBinder @NestedAbstract
