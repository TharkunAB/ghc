{-# LANGUAGE DependentTypes, LinearTypes, ListTuplePuns #-}
{-# LANGUAGE MagicHash, OverloadedRecordDot, TypeFamilies, UnboxedTuples, UndecidableInstances #-}
-- | Executable examples for DepFunTy INLINE unfoldings,
-- promoted Char# and unboxed tuples, superclass selectors in types, same-module
-- closed evaluation, standalone constrained families, runtime Type cases,
-- dependent OverloadedRecordDot, T2T operator sections, GHC.Exists combinators,
-- linear foreach binders, absurd foreach branch refutation, and local type synonyms.
module BookExtensions
  ( inlConsVec
  , IsAsciiA
  , FstUnboxed
  , SwapUnboxed
  , primCharRefl
  , unboxedSwapRefl
  , fibClosed
  , sameModuleFibProof
  , HasItem
  , ItemOf
  , standaloneItemProof
  , Gate (..)
  , SubGate (..)
  , GateTag (..)
  , tagGate
  , subGateTag
  , observeSubGate
  , describeType
  , readCellsViaDot
  , checkDependentDot
  , (+.)
  , congAt
  , congRightPlus
  , congRightCons
  , upperExistsVec
  , SingBool (..)
  , linearSingBool
  , dupInTypeLinear
  , zipEqualVec
  , checkLinearAndRefute
  , localTypeSynCheck
  ) where

import Data.Char (toUpper)
import Data.Kind (Type)
import Data.Nat.Dependent (Nat (Succ, Zero), natToInt)
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality.Dependent ((:~:) (Refl), Matchability)
import Data.Vec.Dependent (Vec (VCons, VNil), mapVec, packListVec, toListVec)
import GHC.Exists
  ( Exists
  , Witness (Witness)
  , mapExists
  , withEvidence
  , withExists
  , data Pack
  , type (/\) (MkCPair)
  )
import GHC.Exts (Char (C#))
import GHC.Sigma (first, second)
import Numeric.Natural (Natural)
import qualified PackedVecLibrary as PV
import ParamColumns (Columns (Columns), cells, storedLength)

-- | Prepend one element to a length-indexed vector with an exported DepFunTy INLINE template.
{-# INLINE inlConsVec #-}
inlConsVec :: foreach (n :: Nat) -> a -> Vec a n -> Vec a (Succ n)
inlConsVec _ x xs = VCons x xs

-- | Match a promoted primitive character literal inside a promoted Char constructor.
type IsAsciiA :: Char -> Bool
type family IsAsciiA c where
  IsAsciiA ('C# 'a'#) = True
  IsAsciiA _          = False

-- | Project the first component of a promoted unboxed pair.
type FstUnboxed :: (# a, b #) -> a
type family FstUnboxed p where
  FstUnboxed (# x, _ #) = x

-- | Swap the two components of a promoted unboxed pair.
type SwapUnboxed :: (# a, b #) -> (# b, a #)
type family SwapUnboxed p where
  SwapUnboxed (# x, y #) = (# y, x #)

-- | Certify promoted Char# pattern reduction at 'a'.
primCharRefl :: IsAsciiA 'a' :~: True
primCharRefl = Refl

-- | Certify promoted unboxed tuple reduction across SwapUnboxed and FstUnboxed.
unboxedSwapRefl :: FstUnboxed (SwapUnboxed (# False, 'z' #)) :~: 'z'
unboxedSwapRefl = Refl

-- | Compute Fibonacci numbers for same-module closed type evaluation.
fibClosed :: Natural -> Natural
fibClosed 0 = 0
fibClosed 1 = 1
fibClosed n = fibClosed (n - 1) + fibClosed (n - 2)

-- | Evaluate fibClosed 9 in a type signature within the defining module.
sameModuleFibProof :: fibClosed 9 :~: 34
sameModuleFibProof = Refl

-- | Class constraint gating a standalone open type family.
class HasItem c

instance HasItem [a]

-- | Standalone open type family carrying a class constraint in its kind signature.
type ItemOf :: HasItem c => c -> Type
type family ItemOf c
type instance ItemOf [a] = a

-- | Reduce a standalone constrained type family at [Char].
standaloneItemProof :: ItemOf [Char] :~: Char
standaloneItemProof = Refl

-- | Base class with a retained selector method used inside dependent types.
class Gate a where
  gate :: a -> Bool

-- | Subclass whose dictionary projects to Gate in dependent signatures and calls.
class Gate a => SubGate a where
  subCode :: a -> Int

instance Gate Bool where
  gate b = b

instance SubGate Bool where
  subCode b = if b then 1 else 0

-- | Witness tag indexed by a Boolean selector result.
data GateTag (b :: Bool) where
  GateTag :: Bool -> GateTag b

-- | Construct a GateTag at a retained Boolean index.
tagGate :: foreach (b :: Bool) -> GateTag b
tagGate b = GateTag @b b

-- | Form a ClassAppTy result signature directly under a SubGate subclass constraint.
subGateTag :: forall a. SubGate a => foreach (x :: a) -> GateTag (gate x)
subGateTag x = tagGate (gate x)

-- | Observe both the superclass-projected GateTag and the subclass method.
observeSubGate :: SubGate a => a -> (Bool, Int)
observeSubGate x = case subGateTag x of
  GateTag v -> (v, subCode x)

-- | Inspect a runtime Type witness with unadorned type patterns and a catch-all.
describeType :: foreach (a :: Type) -> Maybe a
describeType a = case a of
  Int    -> Just 12
  Bool   -> Just True
  [Char] -> Just "ok"
  _      -> Nothing

-- | Project the vector field of an imported dependent record using OverloadedRecordDot.
readCellsViaDot :: forall a. foreach (r :: Columns a) -> PV.Vec a (storedLength r)
readCellsViaDot r = r.cells

-- | Project both a dependent record and a Sigma pair via OverloadedRecordDot.
checkDependentDot :: String -> (Int, String)
checkDependentDot s =
  let pkg = packListVec s
      col = Columns (PV.S (PV.S PV.Z)) (PV.VCons 'a' (PV.VCons 'b' PV.VNil))
      _   = PV.toList (readCellsViaDot col)
  in (natToInt pkg.first, toListVec pkg.second)

infixl 6 +.

-- | Add two Peano naturals with an infix operator for T2T operator sections.
(+.) :: Nat -> Nat -> Nat
Zero   +. n = n
Succ m +. n = Succ (m +. n)

-- | Lift a propositional equality through a required type-level function argument.
congAt :: forall {a} {b} {m :: Matchability}.
          forall (f :: a %m -> b) -> forall (x :: a) (y :: a).
          x :~: y -> f x :~: f y
congAt _ Refl = Refl

-- | Apply a right operator section (+. p) with a captured foreach binder in T2T.
congRightPlus :: forall (m :: Nat) (n :: Nat).
                 foreach (p :: Nat) -> m :~: n -> (m +. p) :~: (n +. p)
congRightPlus p eq = congAt (+. p) eq

-- | Apply a right list-cons section (: ys) in T2T.
congRightCons :: forall (a :: Type) (x1 :: a) (x2 :: a) (ys :: [a]).
                 x1 :~: x2 -> (x1 : ys) :~: (x2 : ys)
congRightCons eq = congAt (: ys) eq

-- | Transform and eliminate an Exists package with Witness, mapExists, withExists, and withEvidence.
upperExistsVec :: (Bool, String)
upperExistsVec =
  let raw :: Exists (Vec Char)
      raw = Pack (type (Succ (Succ Zero))) (VCons 'a' (VCons 'b' VNil))
      upper :: Exists (Vec Char)
      upper = mapExists (\_ xs -> mapVec toUpper xs) raw
      paired :: Show Bool /\ Bool
      paired = MkCPair True
      witOk = case Witness @Nat of Witness -> True
  in ( withEvidence paired (\b -> b && witOk)
     , withExists upper (\_ xs -> toListVec xs) )

-- | Singleton GADT indexed by a Boolean value.
data SingBool (b :: Bool) where
  STrue  :: SingBool True
  SFalse :: SingBool False

-- | Match a linear foreach Boolean binder at multiplicity One.
linearSingBool :: foreach (b %1 :: Bool) -> SingBool b
linearSingBool True  = STrue
linearSingBool False = SFalse

-- | Use a linear foreach binder twice in a type while consuming it once at runtime.
dupInTypeLinear :: foreach (n %1 :: Nat) -> Proxy '(n, n) -> Nat
dupInTypeLinear n Proxy = n

-- | Zip two vectors at independent foreach lengths when an equality GADT refutes off-diagonal cases.
zipEqualVec :: foreach (m n :: Nat) -> Vec a m -> Vec b n -> (m :~: n) -> Vec (a, b) m
zipEqualVec Zero      Zero      VNil         VNil         Refl = VNil
zipEqualVec (Succ m') (Succ n') (VCons x xs) (VCons y ys) Refl =
  VCons (x, y) (zipEqualVec m' n' xs ys Refl)

-- | Exercise linear foreach binders alongside off-diagonal GADT branch refutation.
checkLinearAndRefute :: (Bool, [(Int, Char)])
checkLinearAndRefute =
  let bOk = case linearSingBool True of STrue -> True
      nOk = natToInt (dupInTypeLinear (Succ Zero) Proxy) == 1
      two = Succ (Succ Zero)
      xs  = VCons (1 :: Int) (VCons 2 VNil)
      ys  = VCons 'a' (VCons 'b' VNil)
  in (bOk && nOk, toListVec (zipEqualVec two two xs ys Refl))

-- | Bind non-recursive local type synonyms inside let and where blocks under ExtendedLet.
localTypeSynCheck :: Int
localTypeSynCheck =
  let type PairOf a = (a, a)
      p :: PairOf Int
      p = (20, 22)
  in addPair (Succ Zero) p
  where
    addPair :: foreach (n :: Nat) -> (Int, Int) -> Int
    addPair n (u, v) =
      let v1 :: LocalVec Int
          v1 = VCons (u + v) (replicateZero n)
      in case v1 of
           VCons total _ -> total
      where
        type LocalVec b = Vec b (Succ n)
        replicateZero :: foreach (m :: Nat) -> Vec Int m
        replicateZero Zero     = VNil
        replicateZero (Succ k) = VCons 0 (replicateZero k)
