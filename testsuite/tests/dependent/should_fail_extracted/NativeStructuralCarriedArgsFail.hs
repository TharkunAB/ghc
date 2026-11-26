{-# LANGUAGE DataKinds, DependentTypes, GADTs, TypeOperators #-}
module NativeStructuralCarriedArgsFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeStructuralCarriedArgsWorker

wrongLocal :: foreach (l :: Label) -> foreach (r :: Label)
           -> carryLast l ('There r) :~: 'There (carryLast ('There l) r)
wrongLocal _ _ = Refl

wrongEmpty :: foreach (l :: Label) -> foreach (r :: Label)
           -> decorate 'Empty l r :~: 'Atom r
wrongEmpty _ _ = Refl

wrongLast :: foreach (l :: Label) -> foreach (t :: Term)
          -> decorateLast l ('Next t) :~: 'Next (decorateLast ('There l) t)
wrongLast _ _ = Refl
