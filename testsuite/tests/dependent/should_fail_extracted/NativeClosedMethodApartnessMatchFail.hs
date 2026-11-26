{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeClosedMethodApartnessMatchFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeClosedMethodApartnessObserved (Box(..), aliasProof, boxed, familyProof, inlinable)
import NativeClosedMethodApartnessOrigin (ImageAlias, ImageFamily, ImageMultiple)

{-# RULES "match/constructor" [~1] forall f. fmap f (Just True) = Nothing #-}

badAlias :: ImageAlias :~: 'Just 'False
badAlias = Refl

badFamily :: ImageFamily :~: 'Just 'False
badFamily = Refl

badMultiple :: ImageMultiple :~: '( 'Just 'False, 'Just 'True)
badMultiple = Refl

useObsAlias :: IO ()
useObsAlias = print aliasProof

useObsFamily :: IO ()
useObsFamily = print familyProof

useObsInlinable :: String
useObsInlinable = inlinable

useObsBox :: IO ()
useObsBox = case boxed of Box p -> print p
