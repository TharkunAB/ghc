{-# LANGUAGE NoDependentTypes, TypeFamilies, TypeOperators #-}
module Main (main) where

import Data.Proxy (Proxy(Proxy))
import Data.Type.Equality ((:~:)(Refl))
import NativeStandaloneConstrainedFamilyProvider

clientElem :: ElemTop [Bool] :~: Bool
clientElem = Refl

clientDelegate :: DelegateTop [Bool] :~: [Bool]
clientDelegate = Refl

main :: IO ()
main =
  case ( elemBoolProof
       , delegateBoolProof
       , forwardBoolProof
       , memberBoolProof
       , clientElem
       , clientDelegate
       , polyElem (Proxy :: Proxy [Bool]) True [True]
       ) of
    (Refl, Refl, Refl, Refl, Refl, Refl, ()) -> pure ()
