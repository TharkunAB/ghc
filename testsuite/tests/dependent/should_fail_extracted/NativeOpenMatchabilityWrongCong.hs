{-# LANGUAGE AllowAmbiguousTypes, DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeOpenMatchabilityWrongCong where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (Matchability)
import NativeOpenMatchabilityCongNat (Nat(..), add, addRight)

cong :: forall {a} {b} {m :: Matchability}.
        forall (f :: a %m -> b) -> forall (x :: a) (y :: a).
        x :~: y -> f x :~: f y
cong _ Refl = Refl

wrongAssoc :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
           -> add (add m n) p :~: add m (add n p)
wrongAssoc Z _ _ = Refl
wrongAssoc (S m) n p = cong S (wrongAssoc n m p)

wrongSource :: foreach (m :: Nat) -> foreach (x :: Nat) -> foreach (y :: Nat)
            -> x :~: y -> addRight m x :~: addRight m y
wrongSource m _ _ eq = cong (add m) eq

type Evidence :: forall {m :: Matchability}. (Bool %m -> Type) -> Type
data Evidence p where
    Proof :: forall {m :: Matchability} (p :: Bool %m -> Type). p True -> Evidence p
data Checked (b :: Bool) = Checked
type ReadProof :: Evidence Checked -> Checked True
type family ReadProof proof where
    ReadProof ('Proof field) = field

wrongPreserve :: forall (p :: Checked True) (q :: Checked True). ReadProof ('Proof @Checked p) :~: q
wrongPreserve = Refl
