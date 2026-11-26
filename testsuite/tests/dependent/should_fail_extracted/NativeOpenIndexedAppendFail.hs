{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, KindSignatures,
             PolyKinds, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenIndexedAppendFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import OpenVectorAppend (appendEmptyAt)
import qualified PackedVecLibrary as P

wrongTail :: forall (xs :: P.Vec Bool ('P.S 'P.Z)) (ys :: P.Vec Bool ('P.S 'P.Z)).
             P.appendVec 'P.VNil xs :~: ys
wrongTail = Refl

wrongRuntime :: foreach (xs :: P.Vec Bool 'P.Z) -> foreach (ys :: P.Vec Bool 'P.Z)
             -> P.appendVec 'P.VNil xs :~: ys
wrongRuntime _ _ = Refl

type EmptyLaw a (n :: P.Nat) (xs :: P.Vec a n) = P.appendVec 'P.VNil xs :~: xs

wrongAppendPair :: forall a. foreach (n :: P.Nat) -> foreach (xs :: P.Vec a n)
                -> foreach (ys :: P.Vec a n)
                -> (EmptyLaw a n xs, EmptyLaw a n ys)
wrongAppendPair n xs ys = case n of
  P.Z -> (appendEmptyAt @a @'P.Z xs, appendEmptyAt @a @'P.Z xs)
  P.S k -> (appendEmptyAt @a @('P.S k) xs, appendEmptyAt @a @('P.S k) ys)

wrongConsAt :: foreach (x :: Bool) -> foreach (y :: Bool) -> foreach (xs :: P.Vec Bool n)
            -> foreach (ys :: P.Vec Bool m)
            -> P.appendVec ('P.VCons x xs) ys :~: 'P.VCons y (P.appendVec xs ys)
wrongConsAt _ _ _ _ = Refl

opaqueLeft :: forall (left :: P.Vec Bool 'P.Z) (xs :: P.Vec Bool ('P.S 'P.Z)).
              Proxy left -> P.appendVec left xs :~: xs
opaqueLeft _ = Refl

wrongHead :: forall a (n :: P.Nat) (m :: P.Nat) (x :: a) (x' :: a)
                    (xs :: P.Vec a n) (ys :: P.Vec a m).
             Proxy xs -> Proxy ys ->
             P.appendVec ('P.VCons x xs) ys :~: 'P.VCons x' (P.appendVec xs ys)
wrongHead _ _ = Refl

wrongCarried :: forall a (n :: P.Nat) (m :: P.Nat) (x :: a)
                       (xs :: P.Vec a n) (ys :: P.Vec a m) (zs :: P.Vec a m).
                Proxy xs -> Proxy ys -> Proxy zs ->
                P.appendVec ('P.VCons x xs) ys :~: 'P.VCons x (P.appendVec xs zs)
wrongCarried _ _ _ = Refl
