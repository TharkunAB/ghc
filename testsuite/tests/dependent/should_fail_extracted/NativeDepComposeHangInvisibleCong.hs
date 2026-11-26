{-# LANGUAGE GHC2024, DataKinds, DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeDepComposeHangInvisibleCong where
import Data.Type.Equality ((:~:) (Refl))
import GHC.Matchability (Matchability)
import NativeDepComposeListDefs (Nat (..), foldr, plus)
import Prelude ()
congInv :: forall {a} {b} {m :: Matchability} (f :: a %m -> b) x y. (x :~: y) -> f x :~: f y
congInv Refl = Refl
step :: foreach (x :: Nat) -> foreach (xs :: [Nat]) -> foreach (z :: Nat)
     -> foldr plus z xs :~: plus (foldr plus Zero xs) z
     -> foldr plus z (x : xs) :~: plus (foldr plus Zero (x : xs)) z
step x _ _ ih = congInv (plus x) ih
