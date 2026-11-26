{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, ScopedTypeVariables,
             TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenBuildRuleFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import OpenBuildSource (Swap)
import qualified OpenBuildWorker as B

badConcrete :: Swap '( 'True, 'False) :~: '( 'False, 'True)
badConcrete = Refl

badMatching :: forall (a :: Bool) (b :: Bool).
               Proxy a -> Proxy b -> B.swapPair '(a, b) :~: '(b, a)
badMatching _ _ = Refl

{-# RULES "use the input pair" forall (p :: (Bool, Bool)). B.swapPair p = p #-}
