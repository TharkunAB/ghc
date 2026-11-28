{-# LANGUAGE DataKinds, NoDependentTypes, TypeOperators #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import NativeDataFamilyNewtypeExistentialProvider

main :: IO ()
main =
  case (unwrapProof, boxKindProof, boxPayloadProof, retainedWrapProof) of
    (Refl, Refl, Refl, Refl) -> pure ()
