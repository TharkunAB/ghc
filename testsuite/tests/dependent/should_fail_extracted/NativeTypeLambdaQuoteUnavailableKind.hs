{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, ScopedTypeVariables #-}
module NativeTypeLambdaQuoteUnavailableKind where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))

unavailableKind :: forall (f :: forall k. k -> Type). Proxy (f Bool) -> Bool
unavailableKind _ = const True witness
  where
    witness :: Proxy ((\cell -> (f 'True, cell)) Bool)
    witness = Proxy
