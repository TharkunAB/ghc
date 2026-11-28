{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, ScopedTypeVariables, TypeApplications #-}
module Main where
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import NamedSplitClient (observeNamedSplitIO)
import NativeNamedSplitAppend ()
import NativeNamedSplitAppendQL ()
import PackedVecLibrary (CheckedSplit, Nat(..), Vec(..), splitAtVecChecked, toList)

keepLazy :: CheckedSplit Bool ('S 'Z) total -> Vec Bool total -> Bool
keepLazy (Pair _ ~(_, _ :: Vec Bool suffix, _)) _ = True

defer :: CheckedSplit Bool ('S 'Z) total -> Vec Bool total -> [Bool]
defer (Pair _ ~(_, _ :: Vec Bool suffix, proof)) second =
    case proof of
        Refl -> case second of
            (xs :: Vec Bool ('S suffix)) -> toList @Bool @('S suffix) xs

main :: IO ()
main = do
    ref <- newIORef (VCons True (VCons False VNil))
    let obtain = splitAtVecChecked (S Z) (S (S Z)) <$> readIORef ref
        later = VCons False (VCons True VNil)
        bottomTuple = Pair (S Z) (error "unused tuple") :: CheckedSplit Bool ('S 'Z) ('S ('S 'Z))
        badProof = Pair (S Z) (VCons True VNil, VCons False VNil, error "split proof forced")
                   :: CheckedSplit Bool ('S 'Z) ('S ('S 'Z))
        bottomSuffix = Pair (S Z) (VCons True VNil, error "unused suffix", Refl)
                     :: CheckedSplit Bool ('S 'Z) ('S ('S 'Z))
    result <- observeNamedSplitIO obtain later
    prefix <- fmap (fmap (\(_, cells, _) -> take 1 cells))
                   (observeNamedSplitIO (pure (Just bottomSuffix)) later)
    deferred <- fmap (fmap (`defer` later)) obtain
    failed <- try @ErrorCall (evaluate (defer badProof later))
    case (result, prefix, deferred, keepLazy bottomTuple later, failed) of
        (Just (1, [True, False], [False, True]), Just [True], Just [False, True], True, Left err)
            | takeWhile (/= '\n') (displayException err) == "split proof forced" -> putStrLn "ok"
        _ -> fail "checked split lost its suffix or changed when the lazy patterns demand it"
