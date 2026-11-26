{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeClosedMethodApartnessCallbackFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeClosedMethodApartnessOrigin (ImageAlias)

{-# RULES "callback/not" [~1] not True = True #-}

badCallback :: ImageAlias :~: 'Just 'False
badCallback = Refl
