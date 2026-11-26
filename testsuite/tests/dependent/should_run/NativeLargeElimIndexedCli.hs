{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds, TypeOperators #-}
module NativeLargeElimIndexedCli where

import Data.Type.Equality ((:~:) (Refl))
import NativeLargeElimIndexedProv

useBoxBot :: forall (n :: Nat) (b :: Box n). logVLamBox 'Bot b
useBoxBot = ()

useAdeqArr :: forall (n :: Nat) (v :: Val) (w :: Val) (e :: Env n) (b :: UTerm ('Succ n)).
              logVLamAdeq ('Arr v w) e b :~: Witness v w e b
useAdeqArr = Refl

useClosBot :: forall (c :: Clos). logVLamClos 'Bot c
useClosBot = ()

useClosJoin :: forall (u :: Val) (v :: Val) (c :: Clos).
               logVLamClos u c -> logVLamClos v c -> logVLamClos ('Join u v) c
useClosJoin x y = (x, y)
