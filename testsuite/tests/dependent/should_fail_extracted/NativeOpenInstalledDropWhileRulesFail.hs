{-# LANGUAGE GADTs, NoDependentTypes #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules #-}
module NativeOpenInstalledDropWhileRulesFail where

import Data.Type.Equality ((:~:)(Refl))
import PreludeTakeWhile (PrefixStep(..), RuntimePrefix(..))

observeWithRules :: RuntimePrefix -> String
observeWithRules (RuntimePrefix gate x xs step) = case step of
  Stops Refl -> case takeWhile (gate &&) (x : xs) of
    [] -> "stop"
    _ -> "bad"
  Keeps Refl -> "keep"
