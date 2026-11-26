{-# LANGUAGE DataKinds, NoDependentTypes, TypeOperators #-}
module NativeBinaryEnumDemandFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeBinaryEnumPrefix (StrictTrue)

badDemand :: Proxy b -> StrictTrue b :~: 'False
badDemand _ = Refl
