{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             ScopedTypeVariables, StandaloneKindSignatures, TypeOperators #-}
module NativeOpenConstructorHelpers where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeOpenConstructorHelpersBridge as B
import qualified NativeOpenConstructorHelpersProvider as P
import NativeOpenConstructorHelpersResidual
  ( Erase, Fill, First, Marked, Second, Wrapped )
import NativeOpenIndexedSubstSyntax (Ctx(..), Name, SomeTerm, Sort(..), Term)
import qualified NativeOpenIndexedSubstSyntax as S
import qualified SpineWorker as W

empty :: First 'W.Empty :~: 'Nothing
empty = Refl

more :: forall (a :: Bool) (xs :: W.Spine Bool).
        Proxy a -> Proxy xs -> First ('W.More a xs) :~: 'Just a
more _ _ = Refl

wrapped :: forall (a :: Bool) (xs :: W.Spine Bool).
           Proxy a -> Proxy xs -> Wrapped ('W.More a xs) :~: 'Just ('Just a)
wrapped _ _ = Refl

marked :: forall (a :: Bool) (xs :: W.Spine Bool).
          Proxy a -> Proxy xs -> Marked ('W.More a xs) :~: 'Just ('W.Marked a)
marked _ _ = Refl

singleton :: forall (a :: Bool). Proxy a -> Second ('W.More a 'W.Empty) :~: 'Just a
singleton _ = Refl

second :: forall (a :: Bool) (b :: Bool) (xs :: W.Spine Bool).
          Proxy a -> Proxy b -> Proxy xs -> Second ('W.More a ('W.More b xs)) :~: 'Just b
second _ _ _ = Refl

erasedEmpty :: Erase 'W.Empty :~: 'W.Empty
erasedEmpty = Refl

erasedMore :: forall (x :: Bool) (xs :: W.Spine Bool).
              Proxy x -> Proxy xs -> Erase ('W.More x xs) :~: 'W.More 'True (Erase xs)
erasedMore _ _ = Refl

filledEmpty :: forall (tag :: Maybe Bool). Proxy tag -> Fill 'W.Empty tag :~: 'W.Empty
filledEmpty _ = Refl

filledMore :: forall (x :: Bool) (xs :: W.Spine Bool) (tag :: Maybe Bool).
              Proxy x -> Proxy xs -> Proxy tag -> Fill ('W.More x xs) tag :~: 'W.More tag (Fill xs tag)
filledMore _ _ _ = Refl

type Result :: B.Box Bool -> Maybe Bool
type Result p = B.box p

bridgeProof :: Result ('B.Box ('P.Pick 'False 'True)) :~: 'Just 'True
bridgeProof = Refl

leaf :: forall (g :: Ctx) (a :: Sort) (x :: Term g a).
        S.substOne ('S.Use 'S.Here) x :~: x
leaf = Refl

underBinder :: forall (g :: Ctx) (x :: Name g 'Num).
               S.substOne
                 ('S.Bind ('S.Use ('S.There 'S.Here))
                   :: Term ('Extend g 'Num) ('Fun 'Num 'Num))
                 ('S.Use x)
               :~: 'S.Bind ('S.Use ('S.There x))
underBinder = Refl

packExistential :: forall (x :: Term ('Extend 'Empty 'Num) 'Num).
                   S.packTerm ('S.Here :: Name ('Extend 'Empty 'Num) 'Num) x
                   :~: 'S.Pack 'Refl x
packExistential = Refl

unpackExistential :: forall (n :: Name ('Extend 'Empty 'Num) 'Num)
                            (x :: Term ('Extend 'Empty 'Num) 'Num).
                     S.unpackTerm n ('S.Pack 'Refl x) :~: x
unpackExistential = Refl

unpackUnary :: forall (x :: Term ('Extend 'Empty 'Num) 'Num).
               S.unpackOne ('S.Pack 'Refl x :: SomeTerm ('Extend 'Empty 'Num) 'Num) :~: x
unpackUnary = Refl
