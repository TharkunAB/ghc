{-# LANGUAGE GHC2024, AllowAmbiguousTypes, DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

module Client (mapLaw, zipLaw, addUnder, lengthAppendSingleton, checkAppendSingleton, checkReverse, zipMapLaw, checkZipMap, checkNat, foldrPlusAppend, foldrPlusZero, checkFoldrAndMatchability, checkPublicSurface) where

import Data.Kind (Type)
import qualified HaskLib as H
import qualified HaskLib.List as HL
import HaskLib.Nat (type (+))
import qualified HaskLib.Nat as N

-- | Use the public functions and equality in a new dependent signature.
zipLaw :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
       -> H.size (zip xs ys) H.:~: H.minSize (H.size xs) (H.size ys)
zipLaw xs ys = H.lengthZip xs ys

-- | Appending one element adds one list cell.
lengthAppendSingleton :: forall a. foreach (xs :: [a]) -> foreach (x :: a)
                      -> H.size (xs ++ '[x]) H.:~: 'H.Succ (H.size xs)
lengthAppendSingleton xs x =
    H.lengthAppend xs [x]
        `H.trans` H.plusSucc (H.size xs) H.Zero
        `H.trans` H.cong @'H.Succ (H.plusZero (H.size xs))

-- | Check the proof without inspecting any list elements.
checkAppendSingleton :: [Bool] -> Bool -> Bool
checkAppendSingleton xs x = case lengthAppendSingleton xs x of
    H.Refl -> True

-- | Give the imported map law the same polymorphic callback domain.
mapLaw :: forall a b. foreach (f :: a -> b) -> foreach (xs :: [a])
       -> H.size (map f xs) H.:~: H.size xs
mapLaw = H.lengthMap

-- | Give the imported reverse law the same polymorphic signature.
reverseLaw :: forall a. foreach (xs :: [a]) -> H.size (reverse xs) H.:~: H.size xs
reverseLaw = H.lengthReverse

-- | Check a reverse proof from an ordinary caller.
checkReverse :: [Bool] -> Bool
checkReverse xs = case reverseLaw xs of
    H.Refl -> True

-- | Compose two library proofs. The map proof visits the entire left spine.
zipMapLaw :: forall a b c d. foreach (pre :: a -> b) -> foreach (combine :: b -> c -> d)
           -> foreach (xs :: [a]) -> foreach (ys :: [c])
           -> H.size (zipWith combine (map pre xs) ys) H.:~: H.minSize (H.size xs) (H.size ys)
zipMapLaw pre combine xs ys =
    H.lengthZipWith combine (map pre xs) ys
        `H.trans` H.congFirst @H.minSize @(H.size ys) (H.lengthMap pre xs)

-- | Instantiate the pipeline with different intermediate and input types.
checkZipMap :: (Bool -> Char) -> (Char -> Int -> Ordering) -> [Bool] -> [Int] -> Bool
checkZipMap pre combine xs ys =
    case zipMapLaw pre combine xs ys of H.Refl -> True

-- | Check numeral conversion in proof types and ordinary runtime expressions.
checkNat :: Bool
checkNat = case twoPlusThree of
    H.Refl -> case (infixAddition, zero, retainedThree) of
        (H.Refl, H.Refl, H.Refl) ->
            value (0 :: N.Nat) == 0
                && value (N.plus 2 3) == 5
                && value (2 + 3) == 5
  where
    twoPlusThree :: N.plus 2 3 H.:~: 5
    twoPlusThree = H.Refl

    infixAddition :: (2 + 3 :: N.Nat) H.:~: 5
    infixAddition = H.Refl

    zero :: (0 :: N.Nat) H.:~: 'N.Zero
    zero = H.Refl

    retainedThree :: N.plus 3 'N.Zero H.:~: 3
    retainedThree = N.plusZero 3

    value :: N.Nat -> Int
    value N.Zero = 0
    value (N.Succ n) = 1 + value n

-- | Move a successor through an inner addition under a fixed left operand.
addUnder :: foreach (m :: H.Nat) -> foreach (n :: H.Nat) -> foreach (p :: H.Nat)
         -> H.plus m (H.plus n ('H.Succ p)) H.:~: H.plus m ('H.Succ (H.plus n p))
addUnder m n p =
    H.cong @(H.plus m) (H.plusSucc n p :: H.plus n ('H.Succ p) H.:~: 'H.Succ (H.plus n p))

-- | Exercise Matchability(..) re-export at both constructors.
type ArrowBy (m :: H.Matchability) (a :: Type) (b :: Type) = a %m -> b

matchableCong :: forall (f :: ArrowBy 'H.Matchable N.Nat N.Nat) x y.
                 x H.:~: y -> f x H.:~: f y
matchableCong = H.cong @f

unmatchableCong :: forall (f :: ArrowBy 'H.Unmatchable N.Nat N.Nat) x y.
                   x H.:~: y -> f x H.:~: f y
unmatchableCong = H.cong @f

-- | Checked foldr distributes over list append.
foldrPlusAppend :: foreach (xs :: [N.Nat]) -> foreach (ys :: [N.Nat]) -> foreach (z :: N.Nat)
                -> H.foldr N.plus z (xs ++ ys) H.:~: H.foldr N.plus (HL.foldr N.plus z ys) xs
foldrPlusAppend [] _ _ = H.Refl
foldrPlusAppend (x : xs) ys z = H.cong @(N.plus x) (foldrPlusAppend xs ys z)

-- | Pass checked foldr directly as a retained foreach argument.
foldrPlusZero :: foreach (xs :: [N.Nat])
              -> N.plus (H.foldr N.plus 'N.Zero xs) 'N.Zero H.:~: H.foldr N.plus 'N.Zero xs
foldrPlusZero xs = N.plusZero (H.foldr N.plus N.Zero xs)

checkFoldrAndMatchability :: [N.Nat] -> [N.Nat] -> Bool
checkFoldrAndMatchability xs ys =
    case foldrPlusAppend xs ys N.Zero of
        H.Refl -> case foldrPlusZero xs of
            H.Refl -> case (matchableCong @'N.Succ (H.Refl @'N.Zero), unmatchableCong @(N.plus 'N.Zero) (H.Refl @'N.Zero)) of
                (H.Refl, H.Refl) -> True

-- | Exercise M, U, Pack, Sigma, Vec, Fin, and heterogeneous equality from HaskLib.
type MatchArrow (a :: Type) (b :: Type) = a %H.M -> b
type UnmatchArrow (a :: Type) (b :: Type) = a %H.U -> b
type ErasedVec a = H.Exists (H.Vec a)

packSampleVec :: ErasedVec Char
packSampleVec = H.Pack (type ('H.Succ 'H.Zero)) (H.VCons 'z' H.VNil)

readErasedVec :: ErasedVec Char -> [Char]
readErasedVec pkg = H.withExists pkg (\_ v -> H.toListVec v)

checkPublicSurface :: Bool
checkPublicSurface =
    let congM :: forall (f :: MatchArrow N.Nat N.Nat). 'N.Zero H.:~: 'N.Zero -> f 'N.Zero H.:~: f 'N.Zero
        congM = H.cong @f
        congU :: forall (f :: UnmatchArrow N.Nat N.Nat). 'N.Zero H.:~: 'N.Zero -> f 'N.Zero H.:~: f 'N.Zero
        congU = H.cong @f
        vec2  = H.VCons 'p' (H.VCons 'q' H.VNil)
        sig   = H.Pair (H.Succ (H.Succ H.Zero)) vec2 :: H.SomeVec Char
        het   = H.HRefl :: 'N.Zero H.:~~: 'N.Zero
        mapped :: ErasedVec String
        mapped = H.mapExists (\_ v -> H.mapVec (: []) v) packSampleVec
        evPair = H.MkCPair (17 :: Int) :: Show Int H./\ Int
        witOk  = case H.Witness @Bool of H.Witness -> True
    in case (H.sym (congM @'N.Succ H.Refl), congU @(N.plus 'N.Zero) H.Refl, het) of
        (H.Refl, H.Refl, H.HRefl) ->
            readErasedVec packSampleVec == "z"
                && H.withExists mapped (\_ v -> H.toListVec v) == ["z"]
                && H.withEvidence evPair show == "17"
                && H.discardEvidence evPair == 17
                && witOk
                && H.lookupVec H.FZ vec2 == 'p'
                && H.lookupVec (H.FS H.FZ) vec2 == 'q'
                && H.natToInt (H.first sig) == 2
                && H.toListVec (H.second sig) == "pq"
