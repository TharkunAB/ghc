{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, KindSignatures,
             ScopedTypeVariables, TypeApplications #-}
module NativeOpenIdentityExtra
  ( unqualified, qualified, implicit, keepScope, checkedPair ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import qualified OpenProducer
import Prelude
import qualified Prelude as P

unqualified :: forall a. a -> id a
unqualified x = x

qualified :: forall a. a -> P.id a
qualified x = x

implicit :: forall id a. Proxy (id a)
implicit = Proxy

keepScope :: Proxy (Maybe Int)
keepScope = implicit @Maybe @Int

checkedPair :: forall (p :: (Type, Type)). Proxy p -> Proxy (OpenProducer.pairFirst p)
checkedPair _ = Proxy
