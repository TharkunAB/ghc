{-# LANGUAGE DataKinds, DependentTypes, ExistentialQuantification, GADTs, ListTuplePuns #-}
module NativeRetainedPolyUntouchableGadt where
import Data.Kind (Type)
import qualified NativeRetainedPolyAndInlineGadtProvider as P
data G :: Type -> Type where
  GInt :: Int -> G Int
data Hidden = forall t. Hidden (G t) (G t)
useRetained :: foreach (n :: P.Nat) -> a -> a
useRetained _ x = x
escapeUntouchable h =
  useRetained P.Zero (case (h :: Hidden) of Hidden (GInt _) a -> a)
