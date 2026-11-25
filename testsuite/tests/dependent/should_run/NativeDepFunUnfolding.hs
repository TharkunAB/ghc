{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ListTuplePuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module NativeDepFunUnfolding
  ( Nat (..)
  , Vec (..)
  , Hidden
  , Choose (..)
  , Tagged (..)
  , Pick
  , topZero
  , inlVarApp
  , inlBoolCase
  , inlVecHead
  , inlAliasDefault
  , inlPackVec
  , inlUnpackSum
  , inlTopIndex
  , inlScopedDict
  , inlinableScaleHead
  , inlinablePack
  , inlinableScopedDict
  , recVecSum
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))

data Nat = Z | S Nat

data Vec (a :: Type) (n :: Nat) where
  VNil :: Vec a 'Z
  VCons :: a -> Vec a n -> Vec a ('S n)

type Hidden = exists (a :: Type). (Maybe a, a -> Int)

type family Pick (b :: Bool) :: Type where
  Pick 'True = Int
  Pick 'False = Bool

class Choose a where
  choose :: a -> Bool

instance Choose Bool where
  choose b = b

data Tagged (b :: Bool) where
  Tagged :: Int -> Tagged b

{-# NOINLINE topZero #-}
topZero :: Nat
topZero = Z

{-# NOINLINE takeProxy #-}
takeProxy :: foreach (n :: Nat) -> Proxy n -> Int
takeProxy _ Proxy = 10

{-# NOINLINE tagBoolInt #-}
tagBoolInt :: foreach (b :: Bool) -> Int -> Tagged b
tagBoolInt _ n = Tagged n

{-# INLINE inlVarApp #-}
inlVarApp :: foreach (n :: Nat) -> Int -> Vec Int n -> Vec Int ('S n)
inlVarApp _ x xs = VCons x xs

{-# INLINE inlBoolCase #-}
inlBoolCase :: foreach (b :: Bool) -> Int -> Pick b
inlBoolCase b n = case b of
  True -> n + 1
  False -> False

{-# INLINE inlVecHead #-}
inlVecHead :: foreach (n :: Nat) -> Vec Int ('S n) -> Int
inlVecHead _ (VCons x _) = x

{-# INLINE inlAliasDefault #-}
inlAliasDefault :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
inlAliasDefault n xs = case n of
  Z -> xs
  _ -> xs

{-# INLINE inlPackVec #-}
inlPackVec :: foreach (b :: Bool) -> Int -> (Proxy b, Hidden)
inlPackVec b x =
  ( Proxy
  , case b of
      True -> ((Just x, (+ 1)) :: Hidden)
      False -> ((Nothing :: Maybe Int, id) :: Hidden)
  )

{-# INLINE inlUnpackSum #-}
inlUnpackSum :: foreach (b :: Bool) -> Proxy b -> Hidden -> Int
inlUnpackSum b Proxy pv = case b of
  False -> 7
  True -> case pv of
    (val, render) -> maybe 0 render val

{-# INLINE inlTopIndex #-}
inlTopIndex :: foreach (b :: Bool) -> Int -> (Proxy b, Int)
inlTopIndex b n =
  ( Proxy
  , case b of
      False -> n
      True -> takeProxy topZero Proxy + n
  )

{-# INLINE inlScopedDict #-}
inlScopedDict :: forall a. Choose a => foreach (x :: a) -> Int -> Tagged (choose x)
inlScopedDict x k = tagBoolInt (choose x) (k + 10)

{-# INLINABLE inlinableScaleHead #-}
inlinableScaleHead :: forall a. Num a => foreach (n :: Nat) -> a -> Vec a ('S n) -> a
inlinableScaleHead _ factor (VCons x _) = factor * x

{-# INLINABLE inlinablePack #-}
inlinablePack :: foreach (n :: Nat) -> Vec Int n -> Hidden
inlinablePack _ xs = case xs of
  VNil -> ((Nothing :: Maybe Int, id) :: Hidden)
  VCons x _ -> ((Just x, (* 2)) :: Hidden)

{-# INLINABLE inlinableScopedDict #-}
inlinableScopedDict :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
inlinableScopedDict x = tagBoolInt (choose x) 99

{-# INLINABLE recVecSum #-}
recVecSum :: foreach (n :: Nat) -> Vec Int n -> Int
recVecSum Z VNil = 0
recVecSum (S m) (VCons x xs) = x + recVecSum m xs
