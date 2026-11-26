{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, KindSignatures,
             TypeOperators #-}
module NativeOpenBuildFail where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import OpenBuildSource (Make)
import qualified OpenBuildWorker as B

rigidSwap :: forall (p :: (Type, Type)). Proxy p -> B.swapPair p :~: p
rigidSwap _ = Refl

wrongOrder :: forall a b. Proxy a -> Proxy b -> Make a b :~: 'B.Shell b a
wrongOrder _ _ = Refl
