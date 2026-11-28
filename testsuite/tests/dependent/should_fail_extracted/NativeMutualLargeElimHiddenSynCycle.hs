{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeMutualLargeElimHiddenSynCycle where
import Data.Kind (Type)
data Val = Bot | Arr Val Val | Join Val Val
data HaltsV (v :: Val) where
  MkHaltsV :: logVLam v -> HaltsV v
type Sneaky (u :: Val) = logVLam Bot
newtype LogArr (v :: Val) (w :: Val) = MkLogArr (HaltsV v -> HaltsV w)
logVLam :: Val -> Type
logVLam Bot        = type ()
logVLam (Arr v w)  = type (LogArr v w)
logVLam (Join u _) = type (Sneaky u)
