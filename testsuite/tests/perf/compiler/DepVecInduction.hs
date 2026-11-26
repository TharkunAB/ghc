{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

-- | Compile-time benchmark for scaling dependent function telescopes,
-- indexed vector operations, and equality-solver congruence chains.
-- See Note [Scaling dependent telescope benchmark].
module DepVecInduction where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl), (:~~:) (HRefl), trans)

{- Note [Scaling dependent telescope benchmark]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DepVecInduction stresses dependent arrow (foreach) elaboration, Quick Look
impredicative/dependent application checking, typechecker unification, and
coercion optimization across a chain of 64 indexed vector lemmas and
telescope specializations.
-}

-- | Peano natural numbers.
data Nat = Zero | Succ Nat

-- | Bounded indices below n.
type Fin :: Nat -> Type
data Fin n where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

-- | Length-indexed vectors.
type Vec :: Type -> Nat -> Type
data Vec a n where
  VNil  :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- | Structural addition on Nat.
plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

-- | Congruence for Succ.
congSucc :: a :~: b -> Succ a :~: Succ b
congSucc Refl = Refl

-- | Right identity of plus.
plusZeroR :: foreach (n :: Nat) -> plus n Zero :~: n
plusZeroR Zero = Refl
plusZeroR (Succ n) = congSucc (plusZeroR n)

-- | Right successor shift of plus.
plusSuccR :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m (Succ n) :~: Succ (plus m n)
plusSuccR Zero _ = Refl
plusSuccR (Succ m) n = congSucc (plusSuccR m n)

-- | Associativity of plus.
plusAssoc
  :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat)
  -> plus (plus a b) c :~: plus a (plus b c)
plusAssoc Zero _ _ = Refl
plusAssoc (Succ a) b c = congSucc (plusAssoc a b c)

-- | Transport a vector along a length equality.
castVec :: m :~: n -> Vec a m -> Vec a n
castVec Refl xs = xs

-- | Build a vector of runtime length n by repeating one value.
replicateVec :: forall a. foreach (n :: Nat) -> a -> Vec a n
replicateVec Zero _ = VNil
replicateVec (Succ n) x = VCons x (replicateVec @a n x)

-- | Safe Fin-indexed vector lookup.
lookupVec :: Fin n -> Vec a n -> a
lookupVec FZ (VCons x _) = x
lookupVec (FS i) (VCons _ xs) = lookupVec i xs

-- | Map a function over a length-indexed vector.
mapVec :: (a -> b) -> Vec a n -> Vec b n
mapVec _ VNil = VNil
mapVec f (VCons x xs) = VCons (f x) (mapVec f xs)

-- | Pointwise combination of two equal-length vectors.
zipWithVec :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWithVec _ VNil VNil = VNil
zipWithVec f (VCons x xs) (VCons y ys) = VCons (f x y) (zipWithVec f xs ys)

-- | Append two vectors.
appendVec :: Vec a m -> Vec a n -> Vec a (plus m n)
appendVec VNil ys = ys
appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

-- | Tail-recursive length-indexed vector reversal accumulator.
revAccVec :: forall a. foreach (m :: Nat) -> foreach (n :: Nat) -> Vec a m -> Vec a n -> Vec a (plus m n)
revAccVec Zero _ VNil acc = acc
revAccVec (Succ m) n (VCons x xs) acc =
  case plusSuccR m n of
    Refl -> revAccVec @a m (Succ n) xs (VCons x acc)

-- | Length-preserving vector reversal.
reverseVec :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n
reverseVec n xs =
  case plusZeroR n of
    Refl -> revAccVec @a n Zero xs VNil

-- | Double a Peano natural number.
double :: Nat -> Nat
double Zero = Zero
double (Succ n) = Succ (Succ (double n))

-- | Commutativity of plus.
plusComm :: foreach (a :: Nat) -> foreach (b :: Nat) -> plus a b :~: plus b a
plusComm Zero b = case plusZeroR b of Refl -> Refl
plusComm (Succ a) b =
  case plusComm a b of
    Refl -> case plusSuccR b a of
      Refl -> Refl

-- | Double equals self-addition.
doublePlus :: foreach (n :: Nat) -> double n :~: plus n n
doublePlus Zero = Refl
doublePlus (Succ n) =
  case doublePlus n of
    Refl -> case plusSuccR n n of
      Refl -> Refl

-- | Single telescope step reassociating and reversing a triple of vectors.
stepTelescope
  :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
  -> Vec Int m -> Vec Int n -> Vec Int p
  -> Vec Int (plus m (plus n p))
stepTelescope m n p xs ys zs =
  let lhs :: Vec Int (plus m n)
      lhs = appendVec (reverseVec @Int m xs) (reverseVec @Int n ys)
      combined :: Vec Int (plus (plus m n) p)
      combined = appendVec lhs (reverseVec @Int p zs)
  in castVec (plusAssoc m n p) combined

tele0 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele0 n xs = reverseVec @Int n xs

tele1 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele1 n xs = stepTelescope Zero Zero n VNil VNil (tele0 n xs)

tele2 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele2 n xs = stepTelescope Zero Zero n VNil VNil (tele1 n xs)

tele3 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele3 n xs = stepTelescope Zero Zero n VNil VNil (tele2 n xs)

tele4 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele4 n xs = stepTelescope Zero Zero n VNil VNil (tele3 n xs)

tele5 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele5 n xs = stepTelescope Zero Zero n VNil VNil (tele4 n xs)

tele6 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele6 n xs = stepTelescope Zero Zero n VNil VNil (tele5 n xs)

tele7 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele7 n xs = stepTelescope Zero Zero n VNil VNil (tele6 n xs)

tele8 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele8 n xs = stepTelescope Zero Zero n VNil VNil (tele7 n xs)

tele9 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele9 n xs = stepTelescope Zero Zero n VNil VNil (tele8 n xs)

tele10 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele10 n xs = stepTelescope Zero Zero n VNil VNil (tele9 n xs)

tele11 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele11 n xs = stepTelescope Zero Zero n VNil VNil (tele10 n xs)

tele12 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele12 n xs = stepTelescope Zero Zero n VNil VNil (tele11 n xs)

tele13 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele13 n xs = stepTelescope Zero Zero n VNil VNil (tele12 n xs)

tele14 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele14 n xs = stepTelescope Zero Zero n VNil VNil (tele13 n xs)

tele15 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele15 n xs = stepTelescope Zero Zero n VNil VNil (tele14 n xs)

tele16 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele16 n xs = stepTelescope Zero Zero n VNil VNil (tele15 n xs)

tele17 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele17 n xs = stepTelescope Zero Zero n VNil VNil (tele16 n xs)

tele18 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele18 n xs = stepTelescope Zero Zero n VNil VNil (tele17 n xs)

tele19 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele19 n xs = stepTelescope Zero Zero n VNil VNil (tele18 n xs)

tele20 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele20 n xs = stepTelescope Zero Zero n VNil VNil (tele19 n xs)

tele21 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele21 n xs = stepTelescope Zero Zero n VNil VNil (tele20 n xs)

tele22 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele22 n xs = stepTelescope Zero Zero n VNil VNil (tele21 n xs)

tele23 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele23 n xs = stepTelescope Zero Zero n VNil VNil (tele22 n xs)

tele24 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele24 n xs = stepTelescope Zero Zero n VNil VNil (tele23 n xs)

tele25 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele25 n xs = stepTelescope Zero Zero n VNil VNil (tele24 n xs)

tele26 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele26 n xs = stepTelescope Zero Zero n VNil VNil (tele25 n xs)

tele27 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele27 n xs = stepTelescope Zero Zero n VNil VNil (tele26 n xs)

tele28 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele28 n xs = stepTelescope Zero Zero n VNil VNil (tele27 n xs)

tele29 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele29 n xs = stepTelescope Zero Zero n VNil VNil (tele28 n xs)

tele30 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele30 n xs = stepTelescope Zero Zero n VNil VNil (tele29 n xs)

tele31 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele31 n xs = stepTelescope Zero Zero n VNil VNil (tele30 n xs)

tele32 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele32 n xs = stepTelescope Zero Zero n VNil VNil (tele31 n xs)

tele33 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele33 n xs = stepTelescope Zero Zero n VNil VNil (tele32 n xs)

tele34 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele34 n xs = stepTelescope Zero Zero n VNil VNil (tele33 n xs)

tele35 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele35 n xs = stepTelescope Zero Zero n VNil VNil (tele34 n xs)

tele36 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele36 n xs = stepTelescope Zero Zero n VNil VNil (tele35 n xs)

tele37 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele37 n xs = stepTelescope Zero Zero n VNil VNil (tele36 n xs)

tele38 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele38 n xs = stepTelescope Zero Zero n VNil VNil (tele37 n xs)

tele39 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele39 n xs = stepTelescope Zero Zero n VNil VNil (tele38 n xs)

tele40 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele40 n xs = stepTelescope Zero Zero n VNil VNil (tele39 n xs)

tele41 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele41 n xs = stepTelescope Zero Zero n VNil VNil (tele40 n xs)

tele42 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele42 n xs = stepTelescope Zero Zero n VNil VNil (tele41 n xs)

tele43 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele43 n xs = stepTelescope Zero Zero n VNil VNil (tele42 n xs)

tele44 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele44 n xs = stepTelescope Zero Zero n VNil VNil (tele43 n xs)

tele45 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele45 n xs = stepTelescope Zero Zero n VNil VNil (tele44 n xs)

tele46 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele46 n xs = stepTelescope Zero Zero n VNil VNil (tele45 n xs)

tele47 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele47 n xs = stepTelescope Zero Zero n VNil VNil (tele46 n xs)

tele48 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele48 n xs = stepTelescope Zero Zero n VNil VNil (tele47 n xs)

tele49 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele49 n xs = stepTelescope Zero Zero n VNil VNil (tele48 n xs)

tele50 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele50 n xs = stepTelescope Zero Zero n VNil VNil (tele49 n xs)

tele51 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele51 n xs = stepTelescope Zero Zero n VNil VNil (tele50 n xs)

tele52 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele52 n xs = stepTelescope Zero Zero n VNil VNil (tele51 n xs)

tele53 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele53 n xs = stepTelescope Zero Zero n VNil VNil (tele52 n xs)

tele54 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele54 n xs = stepTelescope Zero Zero n VNil VNil (tele53 n xs)

tele55 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele55 n xs = stepTelescope Zero Zero n VNil VNil (tele54 n xs)

tele56 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele56 n xs = stepTelescope Zero Zero n VNil VNil (tele55 n xs)

tele57 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele57 n xs = stepTelescope Zero Zero n VNil VNil (tele56 n xs)

tele58 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele58 n xs = stepTelescope Zero Zero n VNil VNil (tele57 n xs)

tele59 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele59 n xs = stepTelescope Zero Zero n VNil VNil (tele58 n xs)

tele60 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele60 n xs = stepTelescope Zero Zero n VNil VNil (tele59 n xs)

tele61 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele61 n xs = stepTelescope Zero Zero n VNil VNil (tele60 n xs)

tele62 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele62 n xs = stepTelescope Zero Zero n VNil VNil (tele61 n xs)

tele63 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele63 n xs = stepTelescope Zero Zero n VNil VNil (tele62 n xs)

tele64 :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
tele64 n xs = stepTelescope Zero Zero n VNil VNil (tele63 n xs)

assocChain1 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain1 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain2 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain2 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain3 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain3 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain4 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain4 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain5 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain5 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain6 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain6 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain7 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain7 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain8 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain8 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain9 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain9 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain10 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain10 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain11 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain11 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain12 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain12 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain13 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain13 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain14 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain14 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain15 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain15 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain16 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain16 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain17 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain17 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain18 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain18 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain19 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain19 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain20 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain20 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain21 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain21 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain22 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain22 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain23 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain23 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain24 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain24 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain25 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain25 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain26 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain26 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain27 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain27 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain28 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain28 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain29 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain29 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain30 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain30 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain31 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain31 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl

assocChain32 :: foreach (a :: Nat) -> foreach (b :: Nat) -> foreach (c :: Nat) -> plus (plus (Succ a) b) (Succ c) :~: Succ (plus a (plus b (Succ c)))
assocChain32 a b c = case plusAssoc (Succ a) b (Succ c) of Refl -> Refl
