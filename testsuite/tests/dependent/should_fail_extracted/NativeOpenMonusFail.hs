{-# LANGUAGE DataKinds, DependentTypes, GADTs, TypeOperators #-}
module NativeOpenMonusFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeOpenMonusSource (monus)
import PreludeMapLengthSize (Nat(..))

wrongSuccessors :: foreach (m :: Nat) -> foreach (n :: Nat)
                -> monus ('Succ m) ('Succ n) :~: 'Succ (monus m n)
wrongSuccessors _ _ = Refl

unknownRight :: foreach (n :: Nat) -> monus 'Zero n :~: 'Zero
unknownRight _ = Refl
