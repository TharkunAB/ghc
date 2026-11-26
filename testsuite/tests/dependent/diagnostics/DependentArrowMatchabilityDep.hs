{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies, UndecidableInstances #-}
module DependentArrowMatchabilityDep where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import DependentArrowMatchability ()
import qualified DependentArrowProvider as P

-- | Infer the callback index with unsaturation in the same module.
type AppliedDefault f = (Proxy f, f Bool)

appliedDefault :: AppliedDefault Maybe :~: (Proxy Maybe, Maybe Bool)
appliedDefault = Refl

type AppliedAbstract = AppliedDefault P.Abstract

appliedAbstract :: AppliedAbstract :~: (Proxy P.Abstract, P.Abstract Bool)
appliedAbstract = Refl
