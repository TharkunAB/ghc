{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module NativeVoidAbsurdMissingRow where
import Data.Type.Equality ((:~:) (Refl))
import Data.Void (Void)
badTo :: Either a Void -> a
badTo (Left x) = x
badProof :: foreach (x :: a) -> badTo (Left x) :~: x
badProof _ = Refl
