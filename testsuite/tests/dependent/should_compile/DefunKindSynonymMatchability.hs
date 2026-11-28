{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, PolyKinds, RankNTypes, TypeAbstractions #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneKindSignatures, TypeApplications #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, TypeOperators, UndecidableInstances #-}
module DefunKindSynonymMatchability where

import Data.Kind (Constraint, Type)
import Data.Type.Equality ((:~:)(Refl))

data TyFun :: Type -> Type -> Type

-- Standalone-kind-signed defunctionalization arrow synonym.
type (~>) :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>

-- Kind-inferred defunctionalization arrow synonym.
type a ~~> b = TyFun a b -> Type
infixr 0 ~~>

type Apply :: (k1 ~> k2) -> k1 -> k2
type family Apply f x

type a @@ b = Apply a b
infixl 9 @@

type ApplyTyConAux1 :: (k1 -> k2) -> (k1 ~> k2)
data ApplyTyConAux1 :: (k1 -> k2) -> (k1 ~> k2)
type instance Apply (ApplyTyConAux1 f) x = f x

type ApplyTyConAux2 :: (k1 -> k2 -> k3) -> (k1 ~> unmatchable_fun)
data ApplyTyConAux2 :: (k1 -> k2 -> k3) -> (k1 ~> unmatchable_fun)

data ApplyInferred :: (k1 -> k2) -> (k1 ~~> k2)

type DemoteSym0 :: Type ~> Type
data DemoteSym0 :: Type ~> Type

type SameKind :: k -> k -> Constraint
type SameKind a b = (() :: Constraint)

type SameKindSym0 :: forall k. k ~> k ~> Constraint
type SameKindSym1 :: forall k. k -> k ~> Constraint
data SameKindSym0 :: forall k. k ~> k ~> Constraint
data SameKindSym1 :: forall k. k -> k ~> Constraint
type instance Apply SameKindSym0 x = SameKindSym1 x
type instance Apply (SameKindSym1 x) y = SameKind x y

type KindOf (x :: k) = k
type KindOfSym0 :: forall k. k ~> Type
data KindOfSym0 :: forall k. k ~> Type
type instance Apply KindOfSym0 x = KindOf x

infixr 0 ~>@#@$, ~>@#@$$, ~>@#@$$$
type (~>@#@$) :: Type ~> Type ~> Type
type (~>@#@$$) :: Type -> Type ~> Type
type (~>@#@$$$) :: Type -> Type -> Type
data (~>@#@$) :: Type ~> Type ~> Type
data (~>@#@$$) :: Type -> Type ~> Type
type x ~>@#@$$$ y = x ~> y
type instance Apply (~>@#@$) x = (~>@#@$$) x
type instance Apply ((~>@#@$$) x) y = x ~> y

type ApplySym0 :: forall a b. (a ~> b) ~> a ~> b
type ApplySym1 :: forall a b. (a ~> b) -> a ~> b
data ApplySym0 :: forall a b. (a ~> b) ~> a ~> b
data ApplySym1 :: forall a b. (a ~> b) -> a ~> b
type instance Apply ApplySym0 f = ApplySym1 f
type instance Apply (ApplySym1 f) x = Apply f x

-- Injective Demote family and SingKind instance exercising (~>) at both
-- TypeLevel and KindLevel.
type Sing :: k -> Type
type family Sing @k

type SLambda :: (k1 ~> k2) -> Type
newtype SLambda (f :: k1 ~> k2) =
  SLambda { applySing :: forall t. Sing t -> Sing (f @@ t) }
type instance Sing @(k1 ~> k2) = SLambda

class SingKind k where
  type Demote k = (r :: Type) | r -> k
  fromSing :: Sing (a :: k) -> Demote k
  toSing :: Demote k -> (forall (a :: k). Sing a -> r) -> r

instance (SingKind k1, SingKind k2) => SingKind (k1 ~> k2) where
  type Demote (k1 ~> k2) = Demote k1 -> Demote k2
  fromSing sFun x = toSing x (\sArg -> fromSing (applySing sFun sArg))
  toSing _ _ = error "unused"

checkApplyAux :: Apply (ApplyTyConAux1 Maybe) Int :~: Maybe Int
checkApplyAux = Refl

checkApplySym :: Apply (Apply ApplySym0 (ApplyTyConAux1 Maybe)) Bool :~: Maybe Bool
checkApplySym = Refl
