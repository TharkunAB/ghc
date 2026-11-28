{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, ScopedTypeVariables,
             StandaloneKindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -fomit-interface-pragmas -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenBuildExtra () where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeOpenBuildControls as Ctrl
import NativeOpenBuildDisjointWorker (swapDisjoint)
import NativeOpenBuildVisible (MakeVisible)
import qualified OpenBuildWorker as B

type SlowOne :: Type -> Type -> B.Shell Type Type
type SlowOne a b = Ctrl.slowOne a b

type SlowBoth :: Type -> Type -> B.Shell Type Type
type SlowBoth a b = Ctrl.slowBoth a b

visibleProof :: MakeVisible Bool Char :~: 'B.Shell Bool Char
visibleProof = Refl

disjointProof :: forall (a :: Bool) (b :: Bool).
                 Proxy a -> Proxy b -> swapDisjoint '(a, b) :~: '(b, a)
disjointProof _ _ = Refl

{-# RULES "use the input pair" forall (p :: (Char, Char)). swapDisjoint p = p #-}
