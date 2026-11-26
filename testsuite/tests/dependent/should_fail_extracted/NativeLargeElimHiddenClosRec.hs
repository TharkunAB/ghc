{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, GADTs, KindSignatures, ListTuplePuns #-}
module NativeLargeElimHiddenClosRec (Nat(..), Clos, Val(..), logVLamClos) where
import Data.Kind (Type)
data Nat = Zero | Succ Nat
data Clos where MkClos :: Nat -> Clos
data Val = Bot | Join Val Val
logVLamClos :: Val -> Clos -> Type
logVLamClos Bot        _ = type ()
logVLamClos (Join u v) c = type (logVLamClos u c, logVLamClos v c)
