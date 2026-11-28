{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ListTuplePuns,
             GADTs, PolyKinds, TypeOperators #-}
module NativeCheckedClosureWithheldFail where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeCheckedClosureHiddenProvider as P

badHiddenHelper :: P.substSingle ('P.At 'P.New :: P.Expr ('P.Push 'P.Empty 'P.Nat) 'P.Nat) 'P.Lit :~: 'P.Lit
badHiddenHelper = Refl

badHiddenEnv :: forall (g :: P.Scope) (e :: P.Env g) (i :: P.Fin g). P.lookupHidden e i :~: 'True
badHiddenEnv = Refl
