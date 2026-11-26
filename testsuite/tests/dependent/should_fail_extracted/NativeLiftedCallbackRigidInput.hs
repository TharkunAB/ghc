{-# LANGUAGE DependentTypes, ListTuplePuns, LinearTypes, MagicHash #-}
module NativeLiftedCallbackRigidInput where
import GHC.Exts (TYPE, RuntimeRep(IntRep))
import Data.Type.Equality ((:~:)(Refl))

rigid :: forall (r :: RuntimeRep) (a :: TYPE r). foreach (p :: a -> Bool) -> Bool
rigid _ = False

unlifted :: forall (a :: TYPE IntRep). foreach (p :: a -> Bool) -> Bool
unlifted _ = False

rigidResult :: forall (r :: RuntimeRep) (b :: TYPE r). foreach (p :: Bool -> b) -> Bool
rigidResult _ = False

nestedResult :: forall (r :: RuntimeRep) (b :: TYPE r). foreach (f :: Bool -> Bool -> b)
             -> foreach (p :: Bool) -> f p p :~: f p p
nestedResult _ _ = Refl

nestedLinear :: foreach (f :: Bool -> Bool %1 -> Bool)
             -> foreach (p :: Bool) -> f p p :~: f p p
nestedLinear _ _ = Refl
