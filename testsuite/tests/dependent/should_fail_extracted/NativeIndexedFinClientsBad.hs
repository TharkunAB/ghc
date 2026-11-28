{-# LANGUAGE DataKinds, GADTs, LinearTypes, NoDependentTypes, ScopedTypeVariables, TypeApplications #-}
module NativeIndexedFinClientsBad where
import AlignedList (checkedZipWith, zipWithExact)
import GHC.Types (Multiplicity)
import NativeIndexedFinExtra (Cell(Cell), CheckedList(CheckedList))
import PackedVecLibrary (Vec, toList)
import ParameterizedIndexLibrary (Fin(FZ), indexGeneric, mkFinGeneric)

escapeComposed :: forall n. [Char] -> [Int] -> Maybe (Vec Int n)
escapeComposed left numbers = fmap ((.) id id) (checkedZipWith left numbers (flip const))

wrongCell :: Cell layout -> Cell other
wrongCell (Cell stored payload) = Cell stored payload

sameList :: CheckedList xs -> CheckedList xs -> ()
sameList _ _ = ()
badIndepFields :: [Bool] -> [Bool] -> ()
badIndepFields xs ys = sameList (CheckedList xs) (CheckedList ys)

pairsLen :: a -> a -> b -> b -> [(a, b)]
pairsLen x y u _ = toList (zipWithExact [x, y] [u] (,))

pairsElem :: a -> a -> b -> b -> [(a, b)]
pairsElem x _ u v = toList (zipWithExact [x, u] [u, v] (,))

wrongKindApp :: [Int]
wrongKindApp = toList (zipWithExact @Maybe [1,2] [10,20] (+))

tooManyApp :: [Int]
tooManyApp = toList (zipWithExact @Int @Int @Int @Int [1,2] [10,20] (+))

independentCursor :: [Char] -> [Char] -> Int -> Maybe Char
independentCursor xs ys n = fmap (indexGeneric xs) (mkFinGeneric ys n)

rigidMult :: forall (p :: Multiplicity) a b. (a %p -> b) -> Maybe a -> Maybe b
rigidMult f = fmap f

emptyIdx :: Char
emptyIdx = indexGeneric @Char [] FZ

linearIdx :: [Char] %1 -> Bool
linearIdx xs = case mkFinGeneric xs 0 of { Just _ -> True; Nothing -> False }

rigidIdx :: forall a. a -> Bool
rigidIdx xs = case mkFinGeneric @Char xs 0 of { Just _ -> True; Nothing -> False }
