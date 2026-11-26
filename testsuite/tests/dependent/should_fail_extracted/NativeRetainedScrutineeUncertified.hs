{-# LANGUAGE DependentTypes #-}
module NativeRetainedScrutineeUncertified where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Types (List)
import NativeRetainedScrutineePolyProvider
{-# OPAQUE opaqueId #-}
opaqueId :: List a -> List a
opaqueId xs = foldl (\acc x -> x : acc) [] xs
appendNilR :: forall a. foreach (xs :: List a) -> appList xs [] :~: xs
appendNilR [] = Refl
appendNilR (_ : xs) = case appendNilR xs of Refl -> Refl
bad :: forall a. foreach (ys :: List a) -> Int
bad ys = case appendNilR (opaqueId ys) of
  Refl -> 0
