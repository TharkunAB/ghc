{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TypeApplications, TypeOperators #-}
module NativeOpenPairLinkableMixed where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeOpenPairLinkableWorker as W

direct :: W.choose (Prelude.quotRem 17 5) :~: 3
direct = Refl

nested :: Prelude.id (W.choose (Prelude.quotRem 17 5)) :~: 3
nested = Refl
