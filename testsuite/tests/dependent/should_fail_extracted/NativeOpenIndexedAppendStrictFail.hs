{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, KindSignatures,
             PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -O1 -fno-enable-rewrite-rules #-}
module NativeOpenIndexedAppendStrictFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeOpenIndexedAppendStrictProvider as P

badNil :: forall a (n :: P.Nat) (xs :: P.Vec a n).
          P.appendStrictNil 'P.VNil xs :~: xs
badNil = Refl

okEmptyStrictHead :: forall a (n :: P.Nat) (xs :: P.Vec a n).
                     P.appendStrictHead 'P.VNil xs :~: xs
okEmptyStrictHead = Refl

badConsStrictHead :: forall a (n :: P.Nat) (m :: P.Nat) (x :: a)
                            (xs :: P.Vec a n) (ys :: P.Vec a m).
                     Proxy xs -> Proxy ys ->
                     P.appendStrictHead ('P.VCons x xs) ys :~: 'P.VCons x (P.appendStrictHead xs ys)
badConsStrictHead _ _ = Refl

badConsCarried :: forall a (n :: P.Nat) (m :: P.Nat) (x :: a)
                         (xs :: P.Vec a n) (ys :: P.Vec a m).
                  Proxy xs -> Proxy ys ->
                  P.appendChangeCarried ('P.VCons x xs) ys :~: 'P.VCons x (P.appendChangeCarried xs ys)
badConsCarried _ _ = Refl
