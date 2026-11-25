{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

-- | Ordinary Haskell2010 client exercising the public HaskLib surface.
module OrdinaryInteropClient (checkOrdinaryInterop) where

import Control.Exception (ErrorCall (..), evaluate, try)
import HaskLib

-- | Zip two ordinary lists only when both match the requested length.
zipExactLists :: Nat -> [a] -> [b] -> Maybe [(a, b)]
zipExactLists n xs ys = do
    left  <- fromListExact n xs
    right <- fromListExact n ys
    pure (toListVec (zipVec left right))

-- | Index into an ordinary list after checking the length and position bounds.
lookupExactList :: Nat -> Int -> [a] -> Maybe a
lookupExactList n idx xs = do
    vec <- fromListExact n xs
    pos <- mkFin n idx
    pure (lookupVec pos vec)

-- | Read the stored length of a packed list without forcing its elements.
packedLength :: [a] -> Int
packedLength xs = natToInt (first (packListVec xs))

-- | Read a finite prefix of a packed list through its dependent pair.
packedPrefix :: Int -> [a] -> (Bool, [a])
packedPrefix k xs = case packListVec xs of
    Pair Zero     _ -> (True, [])
    Pair (Succ _) v -> (False, take k (toListVec v))

-- | Eliminate a dependent pair with 'withSigma' at a known result type.
packedLenAndPrefix :: SomeVec a -> (Int, [a])
packedLenAndPrefix pkg = withSigma pkg (\n v -> (natToInt n, take 1 (toListVec v)))

-- | Eliminate a dependent pair with 'withSigma' and 'second'.
packedSecondHead :: [a] -> Maybe a
packedSecondHead xs =
    let pkg = packListVec xs
        viaProj = case second pkg of
            VNil      -> Nothing
            VCons x _ -> Just x
    in case packedLenAndPrefix pkg of
        (0, [])       -> case viaProj of { Nothing -> Nothing; Just _ -> Nothing }
        (_, a : _)    -> case viaProj of { Just _  -> Just a;  Nothing -> Nothing }
        _             -> Nothing

-- | Inspect equality combinators from ordinary Haskell2010 code.
checkEqualityProofs :: [Int] -> [Int] -> Bool
checkEqualityProofs xs ys =
    let revEq    = sym (lengthReverse xs)
        appEq    = lengthAppend xs ys `trans` plusComm (size xs) (size ys)
        appLstEq = lengthAppendList xs ys
        assocEq  = plusAssoc (size xs) (size ys) (Succ Zero)
        intRefl  = Refl :: (:~:) Int Int
        castVal  = rewrite intRefl (gcastWith (sym intRefl) True)
        hetOk    = case HRefl :: (:~~:) Bool Bool of HRefl -> True
    in case (revEq, appEq, appLstEq, assocEq) of
        (Refl, Refl, Refl, Refl) -> castVal && hetOk

-- | Run all ordinary-Haskell boundary, container, and proof checks.
checkOrdinaryInterop :: IO Bool
checkOrdinaryInterop = do
    let two       = Succ (Succ Zero)
        three     = Succ two
        lazyElem1 = error "forced lazy element 1" :: Int
        lazyElem2 = error "forced lazy element 2" :: Int
        lazyTail  = error "forced excess tail" :: [Int]

    -- Excess input on Zero bound must not force head or tail.
    let zeroRejectOk = case fromListExact Zero (lazyElem1 : lazyTail) of
            Nothing -> True
            Just _  -> False

    -- Exact length check must leave elements unevaluated.
    let lazySuccessOk = case fromListExact two [lazyElem1, lazyElem2] of
            Just (VCons _ (VCons _ VNil)) -> True
            _                             -> False

    -- Short input on Succ bound must be rejected cleanly.
    let shortRejectOk = case fromListExact three [10 :: Int, 20] of
            Nothing -> True
            Just _  -> False

    -- Prefix observation on packListVec must not force an infinite/partial tail.
    let (wasEmpty, prefix1) = packedPrefix 1 ('a' : error "forced packed tail")

    -- Forcing an unknown spine before the requested bound must raise its error.
    forcedSpine <- try (evaluate (fromListExact two (1 : error "spine boom" :: [Int])))
    let spineErrorOk = case forcedSpine of
            Left (ErrorCall "spine boom") -> True
            _                             -> False

    let manualVec = VCons 'x' (VCons 'y' (VCons 'z' VNil))
        finOk     = lookupVec FZ manualVec == 'x'
                 && lookupVec (FS (FS FZ)) manualVec == 'z'
                 && mkFinVec manualVec 1 == Just (FS FZ)
                 && mkFinVec manualVec 3 == Nothing
                 && mkFinVec manualVec (-1) == Nothing

    pure $ zeroRejectOk
        && lazySuccessOk
        && shortRejectOk
        && not wasEmpty
        && prefix1 == "a"
        && spineErrorOk
        && finOk
        && zipExactLists two [1 :: Int, 2] ['a', 'b'] == Just [(1, 'a'), (2, 'b')]
        && zipExactLists two [1 :: Int] ['a', 'b'] == Nothing
        && zipExactLists two [1 :: Int, 2, 3] ['a', 'b'] == Nothing
        && lookupExactList three 0 ["alpha", "beta", "gamma"] == Just "alpha"
        && lookupExactList three 2 ["alpha", "beta", "gamma"] == Just "gamma"
        && lookupExactList three 3 ["alpha", "beta", "gamma"] == Nothing
        && lookupExactList three (-1) ["alpha", "beta", "gamma"] == Nothing
        && packedLength [lazyElem1, lazyElem2, lazyElem1] == 3
        && packedSecondHead ([] :: [Int]) == Nothing
        && packedSecondHead [42 :: Int, lazyElem1] == Just 42
        && toListVec (appendVec (VCons (1 :: Int) VNil) (VCons 2 VNil)) == [1, 2]
        && toListVec (mapVec (+ 10) (VCons (1 :: Int) (VCons 2 VNil))) == [11, 12]
        && toListVec (replicateVec two 'k') == "kk"
        && headVec manualVec == 'x'
        && toListVec (tailVec manualVec) == "yz"
        && append [1 :: Int, 2] [3, 4] == [1, 2, 3, 4]
        && checkEqualityProofs [1, 2] [3]
