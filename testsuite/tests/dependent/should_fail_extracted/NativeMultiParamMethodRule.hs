{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module NativeMultiParamMethodRule where

import Data.Type.Equality ((:~:) (Refl))
import NativeMultiParamMethodOpenEqProvider

{-# RULES "tappend/zero" forall x. tappend x MZero = MZero #-}

badRule :: foreach (n :: MyNat) -> tappend MZero n :~: n
badRule _ = Refl
