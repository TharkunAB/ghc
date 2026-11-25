{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns -Wno-inaccessible-code #-}

module Main (main) where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..), (:~~:) (..), type (~~))
import GHC.Exts (Int (I#), Int#, RuntimeRep (..), TYPE)

-- 1. RuntimeRep-indexed GADT (RawInt :: RawEq Int#)
type RawEq :: forall (r :: RuntimeRep). TYPE r -> Type
data RawEq a where
  RawInt :: RawEq Int#

type AtRep :: forall (r :: RuntimeRep). TYPE r -> Type
data AtRep a = MkAtRep Int

generalRaw :: forall r (a :: TYPE r). RawEq a -> AtRep a -> AtRep Int#
generalRaw RawInt x = x

generalRawTwoRows :: forall r (a :: TYPE r). RawEq a -> Bool -> AtRep a -> AtRep Int#
generalRawTwoRows RawInt True x = x
generalRawTwoRows RawInt False x = x

pattern PRawInt :: forall (r :: RuntimeRep) (a :: TYPE r). () => (r ~ 'IntRep, a ~~ Int#) => RawEq a
pattern PRawInt = RawInt

usePRawInt :: forall r (a :: TYPE r). RawEq a -> AtRep a -> AtRep Int#
usePRawInt PRawInt x = x

-- 2. QuantifiedGadtIndex + nested ForAllTy after arrow + higher-rank ForAllTy binder + forgetful synonym
type BoolSlot :: forall (a :: Bool). Proxy a -> Type
data BoolSlot p = MkBoolSlot

type Pick :: forall k. k -> (forall (a :: k). Proxy a -> Type) -> Type
data Pick x f where
  PickBool :: Pick 'True BoolSlot

usePick :: forall k (x :: k) (f :: forall (a :: k). Proxy a -> Type). Pick x f -> f ('Proxy @x) -> Int
usePick PickBool MkBoolSlot = 10

type NestedFam :: Proxy Bool -> forall (a :: Bool). Proxy a -> Type
data NestedFam p q = MkNestedFam Int

type PickNested :: forall k. k -> (Proxy k -> forall (a :: k). Proxy a -> Type) -> Type
data PickNested x f where
  PickNestedBool :: PickNested 'True NestedFam

usePickNested
  :: forall k (x :: k) (f :: Proxy k -> forall (a :: k). Proxy a -> Type)
   . PickNested x f -> f ('Proxy @k) ('Proxy @x) -> Int
usePickNested PickNestedBool (MkNestedFam n) = n

type PolySlot :: forall k -> forall (x :: k). Proxy x -> Type
data PolySlot k p = MkPolySlot

type HigherSlot :: (forall (x :: Bool). Proxy x -> Type) -> Type
data HigherSlot g = MkHigherSlot (g ('Proxy @'True))

type PickHigher :: forall k. k -> ((forall (x :: k). Proxy x -> Type) -> Type) -> Type
data PickHigher x f where
  PickHigherBool :: PickHigher 'True HigherSlot

usePickHigher
  :: forall k (x :: k) (f :: (forall (y :: k). Proxy y -> Type) -> Type)
   . PickHigher x f -> f (PolySlot k) -> Int
usePickHigher PickHigherBool (MkHigherSlot MkPolySlot) = 20

type KeepFirst (a :: Type) (b :: k) = a

type ForgetSyn :: forall k (u :: KeepFirst Type k). k -> u -> Type
data ForgetSyn x u where
  ForgetSynBool :: ForgetSyn 'True Int

useForgetSyn :: forall k (u :: KeepFirst Type k) (x :: k). ForgetSyn x u -> u -> Int
useForgetSyn ForgetSynBool n = n

-- 3. RepBool / RepMaybe (single, nested, multi-row, pattern synonym)
type Rep :: forall k. k -> Type
data Rep a where
  RepBool :: Rep 'True
  RepMaybe :: forall a. Rep a -> Rep ('Just a)

evalRep :: forall k (a :: k). Rep a -> Int
evalRep RepBool = 1
evalRep (RepMaybe r) = 1 + evalRep r

evalRepTwoRows :: forall k (a :: k). Rep a -> Bool -> (a :~~: 'True) -> Int
evalRepTwoRows RepBool True HRefl = 3
evalRepTwoRows RepBool False HRefl = 4
evalRepTwoRows (RepMaybe _) _ _ = 0

pattern PRepBool = RepBool

-- 4. Chained & Either k l kind refinements
type At :: forall k. k -> Type
data At a where
  AtTrue :: At 'True

type Chained :: forall k (a :: k). At a -> Type
data Chained x where
  Chained :: Chained 'AtTrue

useChained :: forall k (a :: k) (x :: At a). Chained x -> (k :~: Bool, a :~~: 'True, x :~~: 'AtTrue)
useChained Chained = (Refl, HRefl, HRefl)

type EitherTag :: forall k l. Either k l -> Type
data EitherTag e where
  LeftTrue :: EitherTag ('Left 'True)

useEitherTag :: forall k l (e :: Either k l). EitherTag e -> l -> (k :~: Bool, l)
useEitherTag LeftTrue y = (Refl, y)

-- 5. Prebuilt base HRefl & solver projections (AppTy, FunTy, ReprEq, TyConApp, inaccessible)
data Nat = Z | S Nat

appHRefl
  :: forall j1 k1 j2 k2 (f :: j1 -> k1) (a :: j1) (g :: j2 -> k2) (b :: j2)
   . (f a :~~: g b) -> (f :~~: g, a :~~: b)
appHRefl HRefl = (HRefl, HRefl)

funHRefl :: forall k (a :: Type) (b :: Type) (x :: k). ((a -> b) :~~: x) -> a -> (a -> b) -> b
funHRefl HRefl v f = f v

newtype Box (a :: k) = MkBox Int

reprCastBox :: forall k l (a :: k) (b :: l). (a :~~: b) -> Coercible (Box a) (Box b) => Box a -> Box b
reprCastBox HRefl = coerce

bothCastedHRefl :: forall k1 k2 (a :: k1) (b :: k2). Rep a -> Rep b -> (a :~~: b) -> Int
bothCastedHRefl RepBool RepBool HRefl = 7
bothCastedHRefl _ _ _ = 0

inaccessibleHRefl :: (('True :: Bool) :~~: ('Z :: Nat)) -> Int
inaccessibleHRefl HRefl = 0
inaccessibleHRefl _ = 1

data Tag (a :: k) = MkTag

projTag :: forall k l (a :: k) (b :: l). (Tag a :~: Tag b) -> (k :~: l, a :~~: b)
projTag Refl = (Refl, HRefl)

main :: IO ()
main = do
  let MkAtRep r1 = generalRaw RawInt (MkAtRep 5)
      MkAtRep r2 = generalRawTwoRows RawInt False (MkAtRep 6)
      MkAtRep r3 = usePRawInt PRawInt (MkAtRep 8)
      p1 = usePick PickBool MkBoolSlot
      p2 = usePickNested PickNestedBool (MkNestedFam 12)
      p3 = usePickHigher PickHigherBool (MkHigherSlot MkPolySlot)
      p4 = useForgetSyn ForgetSynBool 15
      e1 = evalRep (RepMaybe (RepMaybe RepBool))
      e2 = case PRepBool of { RepBool -> evalRepTwoRows RepBool True HRefl; _ -> 0 }
      (Refl, HRefl, HRefl) = useChained Chained
      (Refl, el) = useEitherTag @Bool @Int @('Left 'True) LeftTrue 9
      (HRefl, HRefl) = appHRefl @Type @Type @Type @Type @Maybe @Int @Maybe @Int HRefl
      fnRes = funHRefl @Type @Int @Int @(Int -> Int) HRefl 4 (+ 1)
      MkBox bx = reprCastBox @Bool @Bool @'True @'True HRefl (MkBox 11)
      bc = bothCastedHRefl RepBool RepBool HRefl
      (Refl, HRefl) = projTag @Bool @Bool @'True @'True Refl
      total = r1 + r2 + r3 + p1 + p2 + p3 + p4 + e1 + e2 + el + fnRes + bx + bc
  if total == 114
    then pure ()
    else error ("Unexpected total: " ++ show total)
