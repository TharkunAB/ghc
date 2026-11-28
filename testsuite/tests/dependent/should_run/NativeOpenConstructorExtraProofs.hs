{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, ScopedTypeVariables,
             TypeOperators #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}
module NativeOpenConstructorExtraProofs () where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeOpenConstructorLargerWorker as L
import NativeOpenConstructorResiduals
  ( BoxedFirst, FirstMaybe, Pick, PickNested, Reordered )
import qualified OpenConstructorWorker as W
import qualified SpineWorker as S

leftProof :: Pick ('W.LeftFields Int Bool) :~: Bool
leftProof = Refl

rightProof :: Pick ('W.RightFields Char Int) :~: Char
rightProof = Refl

reorderedProof :: Reordered ('W.LeftFields 'True 'x') :~: 'x'
reorderedProof = Refl

justProof :: forall (x :: Bool) (fallback :: Bool).
             Proxy x -> Proxy fallback -> PickNested ('W.Nested ('Just x)) fallback :~: x
justProof _ _ = Refl

nothingProof :: forall (fallback :: Char).
                Proxy fallback -> PickNested ('W.Nested 'Nothing) fallback :~: fallback
nothingProof _ = Refl

largerProof :: ( L.choose ('L.Larger ('Just 'True) 'False) 'False :~: 'True
               , L.choose 'L.Empty 'False :~: 'False
               , L.project ('L.Product 'True) :~: 'True
               , L.retain 'L.Marker 'False :~: 'False
               , L.innermost ('L.Deep ('Just ('Just 'x'))) 'y' :~: 'x'
               , L.innermost ('L.Deep ('Just 'Nothing)) 'y' :~: 'y' )
largerProof = (Refl, Refl, Refl, Refl, Refl, Refl)

spineEmpty :: FirstMaybe ('S.Empty :: S.Spine Char) :~: 'Nothing
spineEmpty = Refl

spineMore :: forall (x :: Bool) (rest :: S.Spine Bool).
             Proxy x -> Proxy rest -> FirstMaybe ('S.More x rest) :~: 'Just x
spineMore _ _ = Refl

spineBoxed :: forall (x :: Bool) (rest :: S.Spine Bool).
              Proxy x -> Proxy rest -> BoxedFirst ('S.More x rest) :~: 'Just ('Just x)
spineBoxed _ _ = Refl
