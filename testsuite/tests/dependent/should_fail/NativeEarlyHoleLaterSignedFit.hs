{-# LANGUAGE DependentTypes #-}
module NativeEarlyHoleLaterSignedFit where

import Data.Type.Equality ((:~:) (Refl))

data Nat = Zero | Succ Nat

laterNatFn :: Nat -> Nat
laterNatFn n = n

stepId :: Nat -> Nat
stepId Zero     = (_ :: Nat -> Nat) Zero
stepId (Succ k) = Succ (stepId k)

useStepIdInType :: foreach (n :: Nat) -> stepId n :~: stepId n
useStepIdInType _ = Refl
