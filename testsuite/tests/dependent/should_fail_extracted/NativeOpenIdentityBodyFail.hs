{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, ScopedTypeVariables #-}
module NativeOpenIdentityBodyFail where

import qualified OpenProducer
import RuntimeOrdering (Witness(..))

wrongHead :: foreach (p :: Bool -> Ordering) -> foreach (x :: Bool)
          -> foreach (y :: Bool) -> Witness (p x)
wrongHead _ _ y = case p y of
  LT -> Less
  EQ -> Equal
  GT -> Greater

wrongRoute :: forall a b. a -> b -> OpenProducer.routeLeft a b
wrongRoute _ y = y
