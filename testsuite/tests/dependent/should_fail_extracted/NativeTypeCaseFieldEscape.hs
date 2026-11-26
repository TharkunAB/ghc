{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeCaseFieldEscape where
import Data.Proxy (Proxy)

data Escape (d :: Maybe Bool) where
  Escape :: forall (d :: Maybe Bool).
    (case d of { Just bit -> Proxy bit; Nothing -> Proxy bit }) -> Escape d
