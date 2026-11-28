{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TypeApplications, TypeOperators #-}
module NativeOpenPairLinkableNoCode where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeOpenPairLinkableWorker as W
import Numeric.Natural (Natural)

direct :: W.choose (Prelude.quotRem 17 5) :~: 3
direct = Refl

{-# RULES "choose/late" [~1] forall a b. W.choose @Natural (a, b) = b #-}
