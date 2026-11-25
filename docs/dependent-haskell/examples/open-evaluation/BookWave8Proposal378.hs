{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, PolyKinds #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeAbstractions, TypeApplications #-}
-- | Executable Wave 4B through Wave 8 examples for Proposal #378 quantifiers,
-- certified Num numerals, nested foreach patterns, retained GADTs, @Pack,
-- explicit data heralds, polymorphic nested source domains, and dependent-base interop.
module BookWave8Proposal378
  ( Peano (..)
  , plusPeano
  , autoNumPlus
  , replicateInvis
  , theValue
  , classifyLen
  , fallbackVec
  , ShowBox (..)
  , renderShowBox
  , showAtPack
  , packSample
  , Tag (..)
  , proxyDataTag
  , invisDataTag
  , useDataHeraldTag
  , assocProdTo
  , assocProdFrom
  , assocProdRoundTrip
  , dependentBaseSecondLookup
  ) where

import Control.Exception (ArithException (Underflow), throw)
import Data.Fin.Dependent (Fin (FS, FZ))
import Data.Kind (Type)
import Data.Nat.Dependent (Nat (Succ, Zero))
import Data.Proxy (Proxy (Proxy))
import Data.Sigma.Dependent (second)
import Data.Type.Equality ((:~:) (Refl))
import Data.Vec.Dependent (Vec (VCons, VNil), lookupVec, packListVec, toListVec)
import GHC.Exists

-- | User-defined Peano natural with certified ground Num numeral promotion.
data Peano = PZero | PSucc Peano
  deriving (Eq, Show)

-- | Add two Peano naturals structurally.
plusPeano :: Peano -> Peano -> Peano
plusPeano PZero     m = m
plusPeano (PSucc n) m = PSucc (plusPeano n m)

-- | Multiply two Peano naturals structurally.
timesPeano :: Peano -> Peano -> Peano
timesPeano PZero     _ = PZero
timesPeano (PSucc n) m = plusPeano m (timesPeano n m)

instance Num Peano where
  (+) = plusPeano
  (*) = timesPeano
  PZero   - PZero   = PZero
  PSucc n - PZero   = PSucc n
  PSucc n - PSucc m = n - m
  PZero   - PSucc _ = throw Underflow
  negate PZero     = PZero
  negate (PSucc _) = throw Underflow
  abs n = n
  signum PZero     = PZero
  signum (PSucc _) = PSucc PZero
  fromInteger n
    | n < 0     = throw Underflow
    | n == 0    = PZero
    | otherwise = PSucc (fromInteger (n - 1))

-- | Prove 2 + 3 = 5 at the type level using certified Num promotion without FromNatural.
autoNumPlus :: plusPeano 2 3 :~: 5
autoNumPlus = Refl

-- | Replicate an element across an invisible foreach binder using @pat and @arg overrides.
replicateInvis :: forall {a}. foreach (n :: Nat). a -> Vec a n
replicateInvis @Zero     _ = VNil
replicateInvis @(Succ k) x = VCons x (replicateInvis @k x)

-- | Bind a runtime type witness with foreach (a :: Type) and accept unadorned type arguments.
theValue :: foreach (a :: Type) -> a -> a
theValue _ x = x

-- | Sum up to two vector elements using nested foreach constructor patterns.
classifyLen :: foreach (n :: Nat) -> Vec Int n -> Int
classifyLen Zero            VNil                  = 0
classifyLen (Succ Zero)     (VCons x VNil)        = x
classifyLen (Succ (Succ _)) (VCons x (VCons y _)) = x + y

-- | Refine an empty vector branch and preserve all remaining lengths via a catch-all case.
fallbackVec :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
fallbackVec n v = case n of
  Zero -> VNil
  _    -> v

-- | Retained GADT constructor combining existential forall, Show dictionary, and foreach index.
data ShowBox where
  MkShowBox :: forall a. Show a => foreach (n :: Nat) -> Vec a n -> ShowBox

-- | Format the elements stored inside a ShowBox using its dictionary and retained length.
renderShowBox :: ShowBox -> (Int, String)
renderShowBox (MkShowBox n xs) = (natLen n, show (toListVec xs))
  where
    natLen Zero     = 0
    natLen (Succ k) = 1 + natLen k

-- | Eliminate an explicit exists package with @Pack type binder syntax.
showAtPack :: (exists a | (a, a -> String)) -> String
showAtPack (Pack @a (x, render)) = render (x :: a)

-- | Construct an explicit exists package with @Pack type application syntax.
packSample :: exists a | (a, a -> String)
packSample = Pack @Int (19, show)

-- | Punned single-constructor datatype used to exercise the explicit data herald.
data Tag = Tag
  deriving (Eq, Show)

-- | Equate an explicit data herald in type syntax with a ticked promoted constructor.
proxyDataTag :: Proxy (data Tag) :~: Proxy 'Tag
proxyDataTag = Refl

-- | Read an invisible foreach Tag binder via @pat.
invisDataTag :: foreach (t :: Tag). Tag
invisDataTag @t = t

-- | Supply a punned constructor via @(data Tag) without pun warnings.
useDataHeraldTag :: Tag
useDataHeraldTag = invisDataTag @(data Tag)

-- | Helper reassociating a left-nested pair to the right.
assocPairR :: (a, b) -> c -> (a, (b, c))
assocPairR (x, y) z = (x, (y, z))

-- | Reassociate a nested pair domain rightward for open type-level evaluation.
assocProdTo :: ((a, b), c) -> (a, (b, c))
assocProdTo (p, z) = assocPairR p z

-- | Helper reassociating a right-nested pair to the left.
assocPairL :: (b, c) -> a -> ((a, b), c)
assocPairL (y, z) x = ((x, y), z)

-- | Reassociate a nested pair domain leftward for open type-level evaluation.
assocProdFrom :: (a, (b, c)) -> ((a, b), c)
assocProdFrom (x, p) = assocPairL p x

-- | Prove round-trip associativity over a polymorphic nested pair domain.
assocProdRoundTrip :: foreach (p :: ((a, b), c)) -> assocProdFrom (assocProdTo p) :~: p
assocProdRoundTrip ((_, _), _) = Refl

-- | Project a packed vector with Data.Sigma.Dependent.second and index into a 3-vector with lookupVec.
dependentBaseSecondLookup :: [Char] -> (String, Char)
dependentBaseSecondLookup xs =
  let packed = packListVec xs
      elems  = toListVec (second packed)
      sample = VCons 'p' (VCons 'q' (VCons 'r' VNil))
  in (elems, lookupVec (FS FZ) sample)
