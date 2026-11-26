{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, Modifiers,
             PolyKinds, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -O1 -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenListFoldrRuleFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import ListFoldrSource (Folded)
import qualified NativeOpenListFoldrProducer as F
import PreludeAppendLengthSource (Appended)
import PreludeMapLengthProof (Spine(..))

foldAppend :: Proxy k -> Proxy z -> Spine xs -> Proxy ys
           -> Folded k z (Appended xs ys) :~: Folded k (Folded k z ys) xs
foldAppend _ _ Empty _ = Refl
foldAppend k z (Cons xs) ys = case foldAppend k z xs ys of
  Refl -> Refl

localCons :: forall a b (k :: a %U -> b %U -> b) (z :: b) (x :: a) (xs :: [a]).
             Proxy k -> Proxy z -> Proxy x -> Proxy xs
          -> F.local k z (x ': xs) :~: k x (F.local k z xs)
localCons _ _ _ _ = Refl

{-# RULES "foldr/nil" [~1] forall k z xs. F.local k z xs = z #-}
