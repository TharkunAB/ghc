{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeLiftedCallbackFamilyBad where
import CompareAt (Result)
import Data.Type.Equality ((:~:)(Refl))

ambiguousLaw :: forall tag. foreach (f :: Bool -> Result tag) -> foreach (x :: Bool) -> f x :~: f x
ambiguousLaw _ _ = Refl

nonFunction :: foreach (f :: Bool -> Result 'False)
            -> foreach (x :: Bool) -> foreach (y :: Bool) -> f x y :~: f x y
nonFunction _ _ _ = Refl

stuckApply :: forall tag. foreach (f :: Bool -> Result tag)
           -> foreach (x :: Bool) -> foreach (y :: Bool) -> f x y :~: f x y
stuckApply _ _ _ = Refl
