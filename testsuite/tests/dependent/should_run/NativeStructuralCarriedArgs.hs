{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import NativeStructuralCarriedArgsWorker

next :: foreach (t :: Term) -> foreach (l :: Label) -> foreach (r :: Label)
     -> decorate ('Next t) l r :~: 'Next (decorate t l r)
next _ _ _ = Refl

nextLast :: foreach (l :: Label) -> foreach (t :: Term)
         -> decorateLast l ('Next t) :~: 'Next (decorateLast l t)
nextLast _ _ = Refl

wildZero :: foreach (n :: Nat) -> timesWild 'Zero n :~: 'Zero
wildZero _ = Refl

namedZero :: foreach (n :: Nat) -> timesNamed 'Zero n :~: 'Zero
namedZero _ = Refl

main :: IO ()
main = case decorate Empty (error "left label") (error "right label") of
  Empty -> case decorateLast (error "label") Empty of
    Atom _ ->
      case (wildZero Zero, namedZero Zero, timesWild Zero (error "multiplier")) of
        (Refl, Refl, Zero) -> print True
        _ -> print False
    _ -> print False
  _ -> print False
