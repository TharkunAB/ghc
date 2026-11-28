{-# OPTIONS_GHC -Wno-implicit-rhs-quantification #-}
{-# LANGUAGE DataKinds, NoDependentTypes, PolyKinds, StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances #-}
module PromotedConCallbackMatchability where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

type f @@ x = Eval (f x)
infixr 0 @@

data Pure :: a -> Exp a
type instance Eval (Pure x) = x

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval (Pure1 f x) = f x

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance Eval ((g <=< f) x) = Eval (g (Eval (f x)))
infixr 1 <=<

type family (<>) (x :: a) (y :: a) :: a
infixr 6 <>
type family MEmpty :: a

newtype Endo a = Endo (a -> Exp a)

type family UnEndo (e :: Endo a) :: a -> Exp a where
  UnEndo ('Endo f) = f

type instance 'Endo f <> 'Endo g = 'Endo (f <=< g)
type instance MEmpty = 'Endo Pure

data FoldMap :: (a -> Exp m) -> [a] -> Exp m
type instance Eval (FoldMap f '[]) = MEmpty
type instance Eval (FoldMap f (x ': xs)) = Eval (f x) <> Eval (FoldMap f xs)

type FoldrDefault_ f y xs =
  Eval (UnEndo (Eval (FoldMap (Pure1 'Endo <=< Pure1 f) xs)) y)

data ConsSym :: a -> [a] -> Exp [a]
type instance Eval (ConsSym x xs) = x ': xs

data Match j k
  = Match_ j k
  | Is_ (j -> Exp Bool) k
  | Any_ k
  | Else_ (j -> Exp k)

data IsTrue :: Bool -> Exp Bool
type instance Eval (IsTrue b) = b

type family If (b :: Bool) (t :: k) (e :: k) :: k where
  If 'True  t _ = t
  If 'False _ e = e

type family Case_ (ms :: [Match j k]) (a :: j) :: k where
  Case_ ('Is_ p b  ': ms) a = If (p @@ a) b (Case_ ms a)
  Case_ ('Any_ b   ': _ ) _ = b
  Case_ ('Else_ f  ': _ ) a = f @@ a

type Is = ('Is_ :: (j -> Exp Bool) -> k -> Match j k)
type Any = ('Any_ :: k -> Match j k)
type Else = ('Else_ :: (j -> Exp k) -> Match j k)

checkEndo :: Eval (UnEndo (MEmpty <> 'Endo (ConsSym 'True)) '[ 'False ]) :~: '[ 'True, 'False ]
checkEndo = Refl

checkFoldr :: FoldrDefault_ ConsSym '[] '[ Int, Bool ] :~: '[ Int, Bool ]
checkFoldr = Refl

checkCaseIs :: Case_ '[ Is IsTrue 'False, Else Pure ] 'True :~: 'False
checkCaseIs = Refl

checkCaseElse :: Case_ '[ Is IsTrue 'True, Else Pure ] 'False :~: 'False
checkCaseElse = Refl

-- Verify that the runtime Endo constructor keeps an ordinary runtime function field.
runtimeEndo :: Endo Int
runtimeEndo = Endo (\_ _ -> error "unused")
