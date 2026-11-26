{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, KindSignatures,
             ScopedTypeVariables, TypeOperators #-}
module NativeOpenConstructorFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeOpenConstructorResiduals (PickNested)
import qualified OpenConstructorWorker as W

wrong :: forall a b. a -> b -> W.pick ('W.RightFields b a)
wrong x _ = x

nestedDemand :: forall (m :: Maybe ()). Proxy m -> PickNested ('W.Nested m) '() :~: '()
nestedDemand _ = Refl

outerDemand :: forall (p :: W.Nested ()). Proxy p -> PickNested p '() :~: '()
outerDemand _ = Refl
