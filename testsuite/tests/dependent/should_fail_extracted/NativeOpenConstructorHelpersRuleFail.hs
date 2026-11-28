{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             ScopedTypeVariables, StandaloneKindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeOpenConstructorHelpersRuleFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeOpenConstructorHelpersBridge as B
import qualified NativeOpenConstructorHelpersProvider as P
import NativeOpenConstructorHelpersResidual (Erase, Fill)
import NativeOpenIndexedSubstSyntax (Ctx(..), Sort(..), Term)
import qualified NativeOpenIndexedSubstSyntax as S
import qualified SpineWorker as W

erasedMore :: forall (x :: Bool) (xs :: W.Spine Bool).
              Proxy x -> Proxy xs -> Erase ('W.More x xs) :~: 'W.More 'True (Erase xs)
erasedMore _ _ = Refl

{-# RULES "erase/replacement" forall (xs :: W.Spine Bool). W.erase xs = W.Empty #-}

filledMore :: forall (x :: Bool) (xs :: W.Spine Bool) (tag :: Maybe Bool).
              Proxy x -> Proxy xs -> Proxy tag -> Fill ('W.More x xs) tag :~: 'W.More tag (Fill xs tag)
filledMore _ _ _ = Refl

{-# RULES "fill/replacement" forall (xs :: W.Spine Bool) (tag :: Maybe Bool). W.fill xs tag = W.Empty #-}

type Result :: B.Box Bool -> Maybe Bool
type Result p = B.box p

bridgeProof :: Result ('B.Box ('P.Pick 'False 'True)) :~: 'Just 'True
bridgeProof = Refl

{-# RULES "choose/bool" forall (p :: P.Choice Bool). P.choose p = False #-}

substProof :: forall (x :: Term 'Empty 'Num). S.substOne ('S.Use 'S.Here) x :~: x
substProof = Refl

{-# RULES "replace/substitution" forall (x :: Term 'Empty 'Num).
    S.substOne (S.Use S.Here) x = S.Lit #-}
