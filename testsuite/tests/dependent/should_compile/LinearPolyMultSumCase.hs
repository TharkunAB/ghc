{-# LANGUAGE DataKinds, LambdaCase, LinearTypes, NoDependentTypes #-}
{-# LANGUAGE PolyKinds, RankNTypes, TypeFamilies, TypeOperators #-}
module LinearPolyMultSumCase where

import Data.Kind (Type)
import GHC.Exts (Multiplicity)
import GHC.Generics

unPar1L :: forall p (m :: Multiplicity). Par1 p %m -> p
unPar1L (Par1 p) = p

data Digit a
  = One a
  | Two a a
  | Three a a a
  | Four a a a a

class GenericL1 (f :: k -> Type) where
  type RepL1 f :: k -> Type
  to1L :: forall a (m :: Multiplicity). RepL1 f a %m -> f a
  from1L :: forall a (m :: Multiplicity). f a %m -> RepL1 f a

type M0 = 'MetaData "Digit" "LinearPolyMultSumCase" "main" 'False
type MC = 'MetaCons "Con" 'PrefixI 'False
type MS = 'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy

instance GenericL1 (Digit :: Type -> Type) where
  type RepL1 (Digit :: Type -> Type) =
    D1 M0 ((C1 MC (S1 MS Par1) :+: C1 MC (S1 MS Par1 :*: S1 MS Par1))
       :+: (C1 MC (S1 MS Par1 :*: (S1 MS Par1 :*: S1 MS Par1))
        :+: C1 MC ((S1 MS Par1 :*: S1 MS Par1) :*: (S1 MS Par1 :*: S1 MS Par1))))
  to1L = \case
    M1 y -> case y of
      L1 (L1 (M1 (M1 f1))) -> One (unPar1L f1)
      L1 (R1 (M1 (M1 f1 :*: M1 f2))) -> Two (unPar1L f1) (unPar1L f2)
      R1 (L1 (M1 (M1 f1 :*: (M1 f2 :*: M1 f3)))) ->
        Three (unPar1L f1) (unPar1L f2) (unPar1L f3)
      R1 (R1 (M1 ((M1 f1 :*: M1 f2) :*: (M1 f3 :*: M1 f4)))) ->
        Four (unPar1L f1) (unPar1L f2) (unPar1L f3) (unPar1L f4)
  from1L = \case
    One f1 -> M1 (L1 (L1 (M1 (M1 (Par1 f1)))))
    Two f1 f2 -> M1 (L1 (R1 (M1 (M1 (Par1 f1) :*: M1 (Par1 f2)))))
    Three f1 f2 f3 ->
      M1 (R1 (L1 (M1 (M1 (Par1 f1) :*: (M1 (Par1 f2) :*: M1 (Par1 f3))))))
    Four f1 f2 f3 f4 ->
      M1 (R1 (R1 (M1 ((M1 (Par1 f1) :*: M1 (Par1 f2))
                  :*: (M1 (Par1 f3) :*: M1 (Par1 f4))))))
