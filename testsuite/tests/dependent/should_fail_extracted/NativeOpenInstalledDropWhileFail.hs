{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, Modifiers,
             TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenInstalledDropWhileFail where

import Data.Type.Equality ((:~:)(Refl))
import PreludeAppendLengthSource (Appended)
import PreludeDropWhileSource (Rejoined, Suffix)
import PreludeFilterAppendProof (FilterDecisions(..))
import PreludeFilterRuntime (BooleanResult(..), booleanResult)
import PreludeSpanSource (Span)
import PreludeTakeWhile (Enabled, Prefix, PrefixStep(..))

wrongSpanDecision :: FilterDecisions p xs -> Rejoined (Span p xs) :~: xs
wrongSpanDecision Done = Refl
wrongSpanDecision (Decide _ _) = Refl

wrongDropDecision :: FilterDecisions p xs -> Appended (Prefix p xs) (Suffix p xs) :~: xs
wrongDropDecision Done = Refl
wrongDropDecision (Decide _ _) = Refl

wrongTakeHead :: foreach (gate :: Bool) -> foreach (x :: Bool) -> foreach (xs :: [Bool])
              -> PrefixStep (Enabled gate) x xs
wrongTakeHead gate _ _ = case booleanResult (gate && False) of
  IsFalse -> Stops Refl
  IsTrue -> Keeps Refl
