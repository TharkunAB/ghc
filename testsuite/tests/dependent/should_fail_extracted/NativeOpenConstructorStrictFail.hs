{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, KindSignatures #-}
module NativeOpenConstructorStrictFail where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import qualified OpenConstructorWorker as W

blocked :: forall (p :: W.Choice Type Type). Proxy p -> W.demand p
blocked _ = undefined

strict :: forall a b. a -> b -> W.demand ('W.LeftFields a b)
strict _ y = y

badAbstract :: forall (p :: W.Envelope Type). Proxy p -> W.unwrap p
badAbstract _ = undefined
