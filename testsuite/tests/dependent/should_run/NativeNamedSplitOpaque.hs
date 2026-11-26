{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies, UndecidableInstances #-}
module NativeNamedSplitOpaque (Fold, HiddenVec, Opaque, Token) where
import PackedVecLibrary (Nat, Vec)
type family Token (n :: Nat)
type Opaque = some (n :: Nat) | Token n
type family Fold (n :: Nat) :: Nat
type HiddenVec = some (n :: Nat) | Vec Bool (Fold n)
