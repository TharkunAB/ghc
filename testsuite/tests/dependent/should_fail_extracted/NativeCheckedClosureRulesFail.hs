{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ListTuplePuns,
             GADTs, Modifiers, PolyKinds, RankNTypes, ScopedTypeVariables,
             TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeCheckedClosureRulesFail where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified NativeCheckedClosureProvider as P
import qualified NativeCheckedClosureDoubleNamed as DN
import qualified NativeCheckedLookupProvider as LP
import qualified NativeCheckedPlugSubstUProvider as UP
import qualified NativeCheckedClosureNamedSubstProvider as NP

type NatScope = P.Push P.Empty P.Nat

openTail :: forall g a b (next :: P.Slot g a) (stamp :: P.Stamp a).
            P.findMarked ('P.Marked ('P.Old next) stamp :: P.Marked (P.Push g b))
              :~: 'P.InTail (P.findMarked ('P.Marked next stamp :: P.Marked g))
openTail = Refl

{-# RULES "findMarked/replace" forall (marked :: P.Marked NatScope).
    P.findMarked marked = P.AtHead (P.Stamp False) #-}

type SubstFn g d = forall a. DN.Slot g a %U -> DN.Expr d a

extFresh :: forall g d b (s :: SubstFn g d).
            DN.extendExpr @g @d @b s (DN.New :: DN.Slot (DN.Push g b) b)
            :~: DN.At DN.New
extFresh = Refl

{-# RULES "local-extend-expr"
    DN.extendExpr (DN.At :: forall a. DN.Slot DN.Empty a -> DN.Expr DN.Empty a)
                  (DN.New :: DN.Slot (DN.Push DN.Empty DN.Nat) DN.Nat) = DN.Lit #-}

lookupRule :: LP.lookupEnv ('LP.Cons 'True 'LP.Nil) 'LP.FZ :~: 'True
lookupRule = Refl

{-# RULES "override checked lookup" forall e i. LP.lookupEnv e i = True #-}

plugRule :: forall (n :: UP.Nat) (w :: UP.UTerm n). UP.substSingleU (UP.UVar UP.FZ) w :~: w
plugRule = Refl

{-# RULES "override substSingleU" forall b (w :: UP.UTerm n). UP.substSingleU b w = w #-}

namedSubstRule :: forall g a (v :: NP.Term g a). NP.substSingle (NP.Var NP.Z) v :~: v
namedSubstRule = Refl

{-# RULES "spoofSingle" forall w. NP.singleSubst w NP.Z = NP.Zero #-}
