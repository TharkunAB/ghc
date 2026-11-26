{-# LANGUAGE DataKinds, DependentTypes, GADTs, TypeOperators #-}
module NativeIndependentConstructorRowsFail where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeIndependentConstructorRowsGuarded as G
import qualified NativeIndependentConstructorRowsIndep as I

unknownIndep :: foreach (x :: I.Label) -> foreach (v :: I.Term)
             -> I.decorate ('I.Atom x) x v :~: v
unknownIndep _ _ = Refl

unknownGuarded :: foreach (x :: G.Label) -> foreach (v :: G.Term)
               -> G.decorate ('G.Atom x) x v :~: v
unknownGuarded _ _ = Refl

wrongGuard :: foreach (x :: G.Label) -> foreach (l :: G.Label) -> foreach (v :: G.Term)
           -> G.matches l x :~: 'True -> G.decorate ('G.Atom x) l v :~: v
wrongGuard _ _ _ Refl = Refl
