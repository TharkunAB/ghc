{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativePolyNestedHiddenCon (Secret, buildSecret, exposedSig) where

import Data.Type.Equality ((:~:) (Refl))

data Secret a = MkSecret a

buildSecret :: a -> (Secret a, ())
buildSecret x = (MkSecret x, ())

exposedSig :: foreach (x :: a) -> buildSecret x :~: buildSecret x
exposedSig _ = Refl
