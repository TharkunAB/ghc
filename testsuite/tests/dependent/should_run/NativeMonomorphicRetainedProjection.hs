{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeOperators #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..), Vec(..))
import RetainedCharClient (observeProjection, observeReplacement, observeSeparateProjection)
import RetainedCharLength (storedLength)
import RetainedCharVector (CharVector(..))

knownLength :: storedLength ('CharVector ('S 'Z) ('VCons 'r' 'VNil)) :~: 'S 'Z
knownLength = Refl

main :: IO ()
main = case knownLength of
  Refl -> do
    observeProjection >>= print
    observeSeparateProjection >>= print
    observeReplacement >>= print
