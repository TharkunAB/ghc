{-# LANGUAGE DependentTypes, ListTuplePuns, LinearTypes, TypeFamilies #-}
module NativeIndexedFinExtra
  ( CheckedList(..), Tagged(..), Cell(..), EqCheckedList(..), eqChecked
  , Positions(..), ordinaryLengths, lookupAtExplicit, lookupAtOmitted, sameScoped, useFallback
  ) where

import AlignedList (zipWithExact)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..), Vec(..), sourceLength, toList)
import ParameterizedIndexLibrary (Fin(..))

data CheckedList (xs :: [Bool]) where
    CheckedList :: foreach (ys :: [Bool]) -> CheckedList ys

data Tagged (tag :: Ordering) where
    Dynamic :: foreach (tag :: Ordering) -> Tagged tag
    Fixed :: Tagged 'EQ

data Cell (layout :: [Bool]) where
    Cell :: foreach (layout :: [Bool]) -> Int -> Cell layout

data EqCheckedList (xs :: [Bool]) where
    EqCheckedList :: forall xs. foreach (ys :: [Bool]) -> (xs :~: ys) -> EqCheckedList xs

eqChecked :: EqCheckedList '[True]
eqChecked = EqCheckedList [True] Refl

data Positions a b where
    Positions :: foreach (xs :: [a]) -> Fin (sourceLength xs) %1
              -> foreach (ys :: [b]) -> Fin (sourceLength ys) %1 -> Positions a b

ordinaryLengths :: Proxy (xs :: [a]) -> Proxy (ys :: [b])
                -> Proxy (sourceLength xs) -> Proxy (sourceLength ys)
                -> (Proxy (sourceLength xs), Proxy (sourceLength ys))
ordinaryLengths _ _ p q = (p, q)

lookupAtExplicit :: forall n m a. foreach (xs :: [a]) -> (m ~ sourceLength xs, n ~ m) => Fin n %1 -> a
lookupAtExplicit [] i = case i of {}
lookupAtExplicit (x : _) FZ = x
lookupAtExplicit (_ : xs) (FS i) = lookupAtExplicit xs i

lookupAtOmitted :: forall n m a. foreach (xs :: [a]) -> (m ~ sourceLength xs, n ~ m) => Fin n %1 -> a
lookupAtOmitted (x : _) FZ = x
lookupAtOmitted (_ : xs) (FS i) = lookupAtOmitted xs i

sameContents :: foreach (xs :: [Int]) -> foreach (ys :: [Int]) -> (xs ~ ys) => Bool
sameContents _ _ = True

sameScoped :: Bool
sameScoped = let element = 1 :: Int in sameContents [element] [element]

useFallback :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
            -> (sourceLength xs ~ sourceLength ys) => [(a, b)]
useFallback [] [] = []
useFallback xs ys = contents
  where contents = toList (zipWithExact xs ys (,))
