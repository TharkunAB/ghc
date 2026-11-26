{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, NoPolyKinds #-}
{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators #-}
module NativeUnsaturatedReexport (module P, rawProof, directChecked, noPolyProofs) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, Multiplicity(Many), U)
import NativeUnsaturatedProvider as P

type Apply :: FUN @U 'Many Bool Bool -> Bool
type Apply f = f 'True

rawProof :: Apply (Select 'True) :~: 'True
rawProof = Refl

directChecked :: NamedMap @Bool @U @Bool (Select 'True) '[ 'True] :~: '[Select 'True 'True]
directChecked = Refl

type family ApplyOne f x where
  ApplyOne f x = f x

type family ForwardOne f xs where
  ForwardOne f (x ': y ': xs) = ForwardMap f xs

noPolyProofs :: (ApplyOne Maybe Bool :~: Maybe Bool, ForwardOne Maybe '[Int, Bool, Char] :~: '[Maybe Char])
noPolyProofs = (Refl, Refl)
