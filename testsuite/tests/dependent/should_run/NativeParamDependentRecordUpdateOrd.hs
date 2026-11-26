{-# LANGUAGE GADTs, NoDependentTypes, Strict #-}
module NativeParamDependentRecordUpdateOrd (checkOrdinaryStrict) where
import Control.Exception (ErrorCall, evaluate, try)
import NativeParamDependentRecordUpdateProvider

swapElem :: Columns a -> Columns Bool
swapElem r = r { storedLength = S Z, cells = VCons True VNil }

keepWitness :: Columns a -> Columns a
keepWitness r = r { storedLength = storedLength r }

negateCols :: Columns Bool -> Columns Bool
negateCols r = r { cells = mapVec not (cells r) }

checkForced :: a -> IO Bool
checkForced ~v = either (const True) (const False) <$> (try (evaluate v >> pure ()) :: IO (Either ErrorCall ()))

checkOrdinaryStrict :: IO Bool
checkOrdinaryStrict = do
  let r0 = Columns (S Z) (VCons False VNil)
      r1 = negateCols r0
      r2 = swapElem (Columns (S Z) undefined :: Columns Char)
  b1 <- checkForced (keepWitness (Columns Z undefined :: Columns Int))
  let ~badLen = undefined :: Nat
  b2 <- checkForced (keepWitness (Columns badLen undefined :: Columns Int))
  pure (toList (cells r1) == [True] && toList (cells r2) == [True] && not b1 && b2)
