{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module NativeMutualLargeElimRuled where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
data HaltsV (v :: Val) where
  MkHaltsV :: logVLam v -> HaltsV v
newtype LogArr (v :: Val) (w :: Val) = MkLogArr (HaltsV v -> HaltsV w)
logVLam :: Val -> Type
logVLam Bot       = type ()
logVLam (Arr v w) = type (LogArr v w)
{-# RULES "logVLam/Bot" forall x. logVLam x = type () #-}
proof :: logVLam Bot
proof = ()
