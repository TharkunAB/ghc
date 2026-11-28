{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeMutualLargeElimBadHeaderKind where
import Data.Kind (Type)
data Val = Bot | Arr Val Val
data HaltsV (x :: logVLam Bot) where
  MkHaltsV :: HaltsV x
logVLam :: Val -> Type
logVLam Bot       = type ()
logVLam (Arr _ _) = type (HaltsV ())
