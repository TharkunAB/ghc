{-# LANGUAGE DataKinds, DependentTypes, GADTs, ImpredicativeTypes, ListTuplePuns, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeRetainedScrutineePitaSurface (checkSurface) where
import Data.Type.Equality ((:~:)(Refl))
import NativeRetainedScrutineePolyProvider (rev)

appendNilR :: forall a. foreach (xs :: [a]) -> (xs ++ '[]) :~: xs
appendNilR [] = Refl
appendNilR (_ : xs) = case appendNilR xs of Refl -> Refl

withRev :: forall a. foreach (ys :: [a]) -> (rev ys ++ '[]) :~: rev ys
withRev ys = appendNilR (rev ys)

withRevCase :: forall a. foreach (ys :: [a]) -> (rev ys ++ '[]) :~: rev ys
withRevCase ys = case appendNilR (rev ys) of Refl -> Refl

withRevCaseBool :: foreach (ys :: [Bool]) -> (rev ys ++ '[]) :~: rev ys
withRevCaseBool ys = case appendNilR (rev ys) of Refl -> Refl

checkSurface :: Bool
checkSurface =
  let xs :: [Int]
      xs = [1, 2]
      bs :: [Bool]
      bs = [True, False]
  in case (withRev xs, withRevCase xs, withRevCaseBool bs) of
       (Refl, Refl, Refl) -> True
