{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, Modifiers, PolyKinds,
             ScopedTypeVariables, StandaloneKindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenListFoldrStrictFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified NativeOpenListFoldrAmbigMethods as C
import qualified NativeOpenListFoldrProducer as F

type StrictBitStep :: (Bool %U -> Bool %U -> Bool) -> Bool -> [Bool] -> Bool
type StrictBitStep k z xs = F.forceBitStep k z xs

type StrictBitSeed :: (Bool %U -> Bool %U -> Bool) -> Bool -> [Bool] -> Bool
type StrictBitSeed k z xs = F.forceBitSeed k z xs

type StrictPolyStep :: forall a b. (a %U -> b %U -> b) -> b -> [a] -> b
type StrictPolyStep k z xs = F.forceStep k z xs

type StrictPolySeed :: forall a b. (a %U -> b %U -> b) -> b -> [a] -> b
type StrictPolySeed k z xs = F.forceSeed k z xs

ambigCons :: forall a b (k :: a %U -> b %U -> b) (z :: b) (x :: a) (xs :: [a]).
             Proxy k -> Proxy z -> Proxy x -> Proxy xs
          -> C.ambiguous k z (x ': xs) :~: k x (C.ambiguous k z xs)
ambigCons _ _ _ _ = Refl
