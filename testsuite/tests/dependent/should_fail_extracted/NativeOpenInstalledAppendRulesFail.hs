{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules #-}
module NativeOpenInstalledAppendRulesFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import PreludeAppendLengthSource (Appended, Plus)
import PreludeMapLengthProof (Spine(..))
import PreludeMapLengthSource (Count)

lengthAppendRules :: Spine xs -> Proxy ys -> Count (Appended xs ys) :~: Plus (Count xs) (Count ys)
lengthAppendRules Empty _ = Refl
lengthAppendRules (Cons xs) ys = case lengthAppendRules xs ys of
  Refl -> Refl
