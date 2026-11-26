{-# LANGUAGE DataKinds, DependentTypes, GADTs, KindSignatures, ListTuplePuns #-}
{-# LANGUAGE Modifiers, ScopedTypeVariables, StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances #-}
module NativeTypeLambdaEtaReject where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)
import NativeTypeLambdaEtaLib

badCapture :: (Stamp Bool) :~: (\cell -> Stamp Int cell)
badCapture = Refl

badSpineCapture :: Stage (Stamp Int) :~: Stage (\cell -> Stamp Char cell)
badSpineCapture = Refl

badReordered :: (Stamp Int) :~: (\cell -> Stamp cell Int)
badReordered = Refl

badConstant :: (Discard Int) :~: ((\_cell -> Int) :: Type %U -> Type)
badConstant = Refl

badInverse :: forall a b. (Discard Int a ~ Discard Int b) => a :~: b
badInverse = Refl

type Same :: (Type %U -> Type) %U -> (Type %U -> Type) %U -> Bool
type family Same f g where
  Same f f = 'True
  Same _ _ = 'False

badFallback :: Same (Discard Int) ((\_cell -> Int) :: Type %U -> Type) :~: 'False
badFallback = Refl
