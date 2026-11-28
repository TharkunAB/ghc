{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Main (main) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeLambdaEtaLib (Batch(..))
import NativeTypeLambdaEtaProofs
  ( discardForward
  , matched
  , overRoute
  , partialStage
  , reverseMap
  , routed
  , updateNamed
  , visible
  )

fromImportedProof :: forall (xs :: [Type]). Proxy xs -> Int
fromImportedProof witness =
  case reverseMap @Int @xs witness of
    Refl -> visible

fromRoute :: forall (flag :: Bool). Proxy flag -> (Int, Int)
fromRoute _ =
  case (matched, discardForward, partialStage @Int Proxy) of
    (Refl, Refl, Refl) -> case overRoute @Int @flag Proxy of
      Refl -> (fromImportedProof (Proxy @'[Bool]), routed)

main :: IO ()
main =
  if updateNamed @Int @'[Bool] (Batch 40) == 42
    then print (fromRoute (Proxy @'True))
    else fail "unexpected updateNamed result"
