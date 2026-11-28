{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, ScopedTypeVariables,
             TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenConstructorRuleFail where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import qualified OpenConstructorWorker as W

pickCached :: forall a b. a -> b -> W.routeCached ('W.LeftFields a b)
pickCached _ y = y

{-# RULES "constructor/matching" forall (shell :: W.Choice Type Type).
    W.cachedPick shell = undefined #-}

pickDirect :: forall a b. a -> b -> W.pick ('W.LeftFields a b)
pickDirect _ y = y

{-# RULES "sum/matching" forall (shell :: W.Choice Type Type).
    W.pick shell = undefined #-}

nestedProof :: W.pickNested ('W.Nested ('Just 'True)) 'False :~: 'True
nestedProof = Refl

{-# RULES "nested/bool" forall (p :: W.Nested Bool) x.
    W.pickNested p x = x #-}
