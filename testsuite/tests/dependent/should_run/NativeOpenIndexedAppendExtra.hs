{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, KindSignatures,
             RankNTypes, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenIndexedAppendExtra
  ( determined, nested, annotatedField, annotatedEmpty, annotatedCons, extraProofsOk ) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P

type EmptyProof a (n :: P.Nat) (xs :: P.Vec a n) = P.appendVec 'P.VNil xs :~: xs

determined :: forall m a. foreach (cut :: P.Nat) -> foreach (xs :: P.Vec a (P.Add cut m))
           -> (Proxy m, EmptyProof a (P.Add cut m) xs)
determined _ _ = (Proxy, Refl)

nested :: forall a. (forall n. foreach (xs :: P.Vec a n) -> EmptyProof a n xs) -> Int
nested _ = 0

annotatedField :: forall n. foreach (xs :: P.Vec Bool n)
               -> P.appendVec ('P.VCons 'True (xs :: P.Vec Bool n)) 'P.VNil
                  :~: 'P.VCons 'True (P.appendVec xs 'P.VNil)
annotatedField _ = Refl

annotatedEmpty :: forall (n :: P.Nat) (xs :: P.Vec Bool n).
                  P.appendVec ('P.VNil :: P.Vec Bool 'P.Z) xs :~: xs
annotatedEmpty = Refl

annotatedCons :: forall (n :: P.Nat) (m :: P.Nat) (x :: Bool)
                        (xs :: P.Vec Bool n) (ys :: P.Vec Bool m).
                 Proxy xs -> Proxy ys ->
                 P.appendVec ('P.VCons x xs :: P.Vec Bool ('P.S n)) (ys :: P.Vec Bool m)
                   :~: 'P.VCons x (P.appendVec xs ys)
annotatedCons _ _ = Refl

closedCons :: P.appendVec ('P.VCons 'True 'P.VNil) ('P.VCons 'False 'P.VNil)
              :~: 'P.VCons 'True ('P.VCons 'False 'P.VNil)
closedCons = Refl

extraProofsOk :: Bool
extraProofsOk = nested @Bool (\_ -> Refl) == 0 && (case closedCons of Refl -> True)
