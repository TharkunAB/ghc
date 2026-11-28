{-# LANGUAGE AllowAmbiguousTypes, DependentTypes, ListTuplePuns, TypeFamilies, TypeFamilyDependencies #-}
module NativeOpenMatchabilityExtra where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (M, Matchability, U)
import NativeOpenMatchabilityCongNat (Nat(..), add, addRight)
import T14174a (type ElimBool)

witness :: Proxy (ElimBool Proxy True 'Proxy 'Proxy)
witness = Proxy

type family K (flag :: Bool) :: Type
type instance K True = Type -> Type
type instance K False = Bool -> Type
type family Separate (p :: K True) (q :: K False) :: Type
type family Repeat (p :: K True) (q :: K True) :: Type

data Var = Op
type Varianced :: forall (m :: Matchability). Type
type Varianced @m = forall (var :: Var). @m Type
data family Parser :: Varianced
data instance Parser = P
parsed :: Parser @Op
parsed = P
term :: Varianced
term = undefined

type family Select (b :: Bool) = (r :: Type) | r -> b where
    Select False = Bool -> Bool
    Select True = Bool %M -> Bool

type family ArrowMatchability (t :: Type) :: Matchability where
    ArrowMatchability (a %M -> b) = M
    ArrowMatchability (a %U -> b) = U

ordinary :: ArrowMatchability (Select False) :~: U
ordinary = Refl

annotated :: ArrowMatchability (Select True) :~: M
annotated = Refl

cong :: forall {a} {b} {m :: Matchability}.
        forall (f :: a %m -> b) -> forall (x :: a) (y :: a).
        x :~: y -> f x :~: f y
cong _ Refl = Refl

assoc :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
      -> add (add m n) p :~: add m (add n p)
assoc Z _ _ = Refl
assoc (S m) n p = cong S (assoc m n p)

outer :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
      -> foreach (q :: Nat) -> add m (add (add n p) q) :~: add m (add n (add p q))
outer m n p q = cong (add m) (assoc n p q)

data Bin = Nil | O Bin | I Bin

inc :: Bin -> Bin
inc Nil = I Nil
inc (O x) = I x
inc (I x) = O (inc x)

incBase :: inc Nil :~: I Nil
incBase = Refl

underInc :: foreach (x :: Bin) -> foreach (y :: Bin) -> x :~: y -> inc x :~: inc y
underInc _ _ eq = cong inc eq

underRight :: foreach (m :: Nat) -> foreach (x :: Nat) -> foreach (y :: Nat)
           -> x :~: y -> addRight m x :~: addRight m y
underRight m _ _ eq = cong (addRight m) eq

type Evidence :: forall {m :: Matchability}. (Bool %m -> Type) -> Type
data Evidence p where
    Proof :: forall {m :: Matchability} (p :: Bool %m -> Type).
             p True -> Evidence p

data Checked (b :: Bool) = Checked

type ReadProof :: Evidence Checked -> Checked True
type family ReadProof proof where
    ReadProof ('Proof field) = field

preserve :: forall (p :: Checked True). ReadProof ('Proof @Checked p) :~: p
preserve = Refl
