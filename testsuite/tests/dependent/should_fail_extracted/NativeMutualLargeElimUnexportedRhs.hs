{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeMutualLargeElimUnexportedRhs (Val (..), HaltsV (..), logVLam) where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
data HaltsV (v :: Val) where
  MkHaltsV :: logVLam v -> HaltsV v
newtype LogArr (v :: Val) (w :: Val) = MkLogArr (HaltsV v -> HaltsV w)
logVLam :: Val -> Type
logVLam Bot       = type ()
logVLam (Arr v w) = type (LogArr v w)
