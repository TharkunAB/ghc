{-# LANGUAGE CPP, DataKinds, DependentTypes, ListTuplePuns, GADTs, Modifiers, PolyKinds #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators #-}
module NativeTypeLambdaCaptureUProvider where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality ((:~:) (Refl))
import GHC.Exts (M, U)

type LeibnizU (x :: a) (y :: a) = forall (p :: a %U -> Type). p x -> p y

type ConstU (r :: Type) = \z -> r

leibnizUToEq :: forall {a} (x :: a) (y :: a). LeibnizU x y -> (x :~: y)
leibnizUToEq xy = xy @(\z -> x :~: z) Refl

-- Unmodified leibnizUSymAt (inferred {a}, unmatchable p :: a %U -> Type, no AllowAmbiguousTypes)
leibnizUSymAt :: forall {a} (x :: a) (y :: a) (p :: a %U -> Type).
                 LeibnizU x y -> p y -> p x
#ifdef WRONG_LAMBDA_TARGET
leibnizUSymAt xy = xy @(\z -> p z -> p y) (\px -> px)
#else
leibnizUSymAt xy = xy @(\z -> p z -> p x) (\px -> px)
#endif

-- Spelling A: inferred {a} with @x @y
leibnizUSym :: forall {a} (x :: a) (y :: a). LeibnizU x y -> LeibnizU y x
leibnizUSym xy = leibnizUSymAt @x @y xy

-- Spelling B: specified `a` with @a @x @y
leibnizUSymAtSpec :: forall a (x :: a) (y :: a) (p :: a %U -> Type).
                     LeibnizU x y -> p y -> p x
leibnizUSymAtSpec xy = xy @(\z -> p z -> p x) (\px -> px)

leibnizUSymSpec :: forall {a} (x :: a) (y :: a). LeibnizU x y -> LeibnizU y x
leibnizUSymSpec xy = leibnizUSymAtSpec @a @x @y xy

-- Transitivity using an inline type lambda capturing an unmatchable predicate
leibnizUTrans :: forall {a} (x :: a) (y :: a) (z :: a).
                 LeibnizU x y -> LeibnizU y z -> LeibnizU x z
leibnizUTrans xy yz = leibnizUSym yzDone
  where
    step :: forall (p :: (a %U -> Type)). p x -> p z
    step px = yz @(\w -> p w) (xy @(\w -> p w) px)
    yzDone :: LeibnizU z x
    yzDone = leibnizUSym step

-- Multi-arrow, parenthesized, and matchable-arrow captures in local where signatures
type Apply2 :: (a %U -> b %U -> Type) -> a -> b -> Type
type family Apply2 rel x y where
  Apply2 rel x y = rel x y

flipRel :: forall {a} {b} (rel :: (a %U -> (b %U -> Type))) (x :: a) (y :: b).
           Apply2 rel x y -> Apply2 (\u v -> rel v u) y x
flipRel r = go r
  where
    go :: Apply2 (\u v -> rel u v) x y -> Apply2 (\u v -> rel v u) y x
    go v = v

type Apply1 :: (a %U -> Type) -> a -> Type
type family Apply1 f x where
  Apply1 f x = f x

matchableWrap :: forall (f :: Type %M -> Type) (a :: Type).
                 f a -> Apply1 (\x -> f x) a
matchableWrap fa = go fa
  where
    go :: f a -> ((\x -> f x) :: Type %U -> Type) a
    go v = v

-- Forwarding lambda vs named unmatchable family callback (including inside a partial family)
type Stamp :: Type -> Type %U -> Type
type family Stamp tag cell where
  Stamp tag cell = Either tag cell

type Stage :: (Type %U -> Type) -> Type %U -> Type
type family Stage f cell where
  Stage f cell = Maybe (f cell)

data Ticket (f :: Type %U -> Type) = Ticket (f Int)

type StampedTicket :: Type -> Type
type StampedTicket tag = Ticket (Stage (Stamp tag))

type ConstTicket :: Type -> Type
type ConstTicket r = Ticket (Stage (ConstU r))

withCallback :: forall (f :: Type %U -> Type). Proxy f -> Ticket (Stage f) -> Ticket (Stage f)
withCallback _ t = t

forwardPartial :: forall (tag :: Type).
                  Proxy tag -> Ticket (Stage (Stamp tag)) -> Ticket (Stage (\cell -> Stamp tag cell))
forwardPartial _ t = withCallback @(\cell -> Stamp tag cell) Proxy t

forwardCaptureU :: forall (p :: Type %U -> Type).
                   Proxy p -> Ticket (Stage p) -> Ticket (Stage (\cell -> p cell))
forwardCaptureU _ (Ticket x) = Ticket x

#ifdef BAD_INVERSE_U
badInverseU :: forall (p :: Type %U -> Type) a b. (p a ~ p b) => a :~: b
badInverseU = Refl
#endif

#ifdef BAD_RIGID_SKOLEMS_U
badRigidSkolems :: forall (p :: Type %U -> Type) (q :: Type %U -> Type) a.
                   p a -> q a
badRigidSkolems x = x
#endif

#ifdef BAD_FIXED_U_TO_M
badFixedUToM :: (forall (f :: Type %U -> Type). f Bool) -> Maybe Bool
badFixedUToM x = x
#endif
