{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, KindSignatures,
             RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenIndexedAppendSigFail where

import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P

type EmptyLaw a (n :: P.Nat) (xs :: P.Vec a n) = P.appendVec 'P.VNil xs :~: xs

ambiguous :: forall m a. foreach (cut :: P.Nat) -> foreach (xs :: P.Vec a (P.Add cut m))
          -> EmptyLaw a (P.Add cut m) xs
ambiguous _ _ = Refl

wrongIndex :: forall (n :: P.Nat) (xs :: P.Vec Bool n).
              P.appendVec ('P.VNil :: P.Vec Bool ('P.S 'P.Z)) xs :~: xs
wrongIndex = Refl
