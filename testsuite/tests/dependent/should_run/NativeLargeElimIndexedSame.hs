{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExplicitNamespaces, DependentTypes, GADTs, KindSignatures, ListTuplePuns, TypeOperators #-}
module NativeLargeElimIndexedSame where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))

data Nat = Zero | Succ Nat
data Box :: Nat -> Type where
  MkBox :: Box n
data Clos where
  MkClos :: Box n -> Clos
data Val = Bot | Arr Val Val | Join Val Val

logVLam :: forall (n :: Nat). Val -> Box n -> Type
logVLam Bot        _ = type ()
logVLam (Arr _ _)  _ = type Int
logVLam (Join _ _) _ = type Bool

logVLamClos :: Val -> Clos -> Type
logVLamClos Bot        _ = type ()
logVLamClos (Arr _ _)  _ = type Int
logVLamClos (Join u v) c = type (logVLamClos u c, logVLamClos v c)

sameBot :: forall (n :: Nat) (b :: Box n). logVLam 'Bot b
sameBot = ()

sameJoinClos :: forall (u :: Val) (v :: Val) (c :: Clos).
                logVLamClos ('Join u v) c :~: (logVLamClos u c, logVLamClos v c)
sameJoinClos = Refl
