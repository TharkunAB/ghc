{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TypeApplications, TypeOperators #-}
module NativeOpenPairLinkable where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeOpenPairLinkableMixed as Mixed
import qualified NativeOpenPairLinkableWorker as W
import Numeric.Natural (Natural)

direct :: W.choose (Prelude.quotRem 17 5) :~: 3
direct = Refl

nested :: Prelude.id (W.choose (Prelude.quotRem 17 5)) :~: 3
nested = Refl

bothMixed :: (W.choose (Prelude.quotRem 17 5) :~: 3, Prelude.id (W.choose (Prelude.quotRem 17 5)) :~: 3)
bothMixed = (Mixed.direct, Mixed.nested)

{-# RULES "choose/late" [~1] forall a b. W.choose @Natural (a, b) = b #-}
