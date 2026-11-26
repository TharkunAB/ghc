{-# LANGUAGE DataKinds, DependentTypes, GADTs, TypeFamilies, TypeOperators #-}
module NativeRetainedFamilyGivenAll where

import Data.Type.Equality ((:~:)(Refl))
import NativeRetainedFamilyGiven ()
import NativeRetainedTailGiven ()

type family Closed (b :: Bool) :: Bool where
  Closed 'False = 'False

closedGiven :: foreach (b :: Bool) -> Closed b :~: b -> 'Just (Closed b) :~: 'Just b
closedGiven _ Refl = Refl
