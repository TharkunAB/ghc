{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, ScopedTypeVariables,
             TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -fomit-interface-pragmas -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenConstructorDisjointRules () where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeOpenConstructorDisjointWorker as W

directDisjoint :: forall a b. a -> b -> W.pick ('W.LeftFields a b)
directDisjoint _ y = y

throughHelper :: forall a b. a -> b -> W.route ('W.RightFields b a)
throughHelper _ y = y

{-# RULES "constructor/disjoint" forall (shell :: W.Choice Bool Char).
    W.pick shell = 'z' #-}

nestedDisjoint :: W.pickNested ('W.Nested ('Just 'True)) 'False :~: 'True
nestedDisjoint = Refl

{-# RULES "nested/specialized" forall (p :: W.Nested Char) x.
    W.pickNested p x = x #-}
