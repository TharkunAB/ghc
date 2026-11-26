{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-gadt-mono-local-binds #-}
module HaskLibOrdinaryClient (checkOrdinaryInterop) where

import Data.Type.Equality ((:~:) (Refl))
import HaskLibCore
import qualified HaskLibProps as NP
import Prelude (Bool (..), Int, IO, Maybe (..), not, pure, (==), (&&), (+), (*))

checkOrdinaryInterop :: IO Bool
checkOrdinaryInterop = do
  let two   = Succ (Succ Zero)
      three = Succ two
      vec   = VCons 'x' (VCons 'y' VNil)
      okDec = decToBool (decEq two two) && not (decToBool (decEq two three))
      okVec = case fromListExact two "hi" of
        Just v  -> toListVec (appendVec v vec) == "hixy"
        Nothing -> False
      okFin = case mkFin three 1 of
        Just i  -> finToInt (oppositeFin three i) == 1
        Nothing -> False
      okEq  = case (NP.plusComm two three, NP.mulAssoc two three two) of
        (Refl, Refl) -> True
      okSig = withSigma (packListVec "abc") (\n v -> (natToInteger n, toListVec v)) == (3, "abc")
      okDot = ((+ 1) . (* 2)) (5 :: Int) == 11
      okApp = ((+ 3) $ (4 :: Int)) == 7
  pure (okDec && okVec && okFin && okEq && okSig && okDot && okApp)
