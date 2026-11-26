{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module NativeSizeListNoDescentFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeSizeListNoDescentSource (withoutDescent)
import PreludeMapLengthSize (Size)

unchecked :: forall a. foreach (bound :: Size) -> foreach (xs :: [a])
          -> withoutDescent bound xs :~: withoutDescent bound xs
unchecked _ _ = Refl
