{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, PolyKinds,
             ScopedTypeVariables, TypeAbstractions, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenInstalledReverseProofFail where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludeAppendLengthSource (Plus)
import PreludeMapLengthProof (Spine(..))
import PreludeMapLengthSource (Count)
import PreludeReverseLengthSource (ReversedOnto)

missingSucc :: forall a (xs :: [a]) (acc :: [a]). Spine xs -> Proxy acc
            -> Count (ReversedOnto xs acc) :~: Plus (Count xs) (Count acc)
missingSucc Empty _ = Refl
missingSucc (Cons @_ @x xs) _ =
  case missingSucc xs (Proxy @(x ': acc)) of
    Refl -> Refl
