{-# LANGUAGE DependentTypes #-}

-- | Runtime performance benchmark for erased type indices, bounded Fin lookup,
-- existential Exists packages, and intrinsically-typed STLC evaluation.
module Main (main) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Exists (Exists, data Pack, withExists)
import GHC.Tuple (Unit)

-- | Peano natural numbers.
data Nat = Zero | Succ Nat

-- | Bounded vector positions.
type Fin :: Nat -> Type
data Fin n where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

-- | Length-indexed strict Int vectors.
type Vec :: Type -> Nat -> Type
data Vec a n where
  VNil  :: Vec a Zero
  VCons :: !a -> !(Vec a n) -> Vec a (Succ n)

-- | Structural addition on Nat.
plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

congSucc :: a :~: b -> Succ a :~: Succ b
congSucc Refl = Refl

plusZeroR :: foreach (n :: Nat) -> plus n Zero :~: n
plusZeroR Zero = Refl
plusZeroR (Succ n) = congSucc (plusZeroR n)

plusSuccR :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m (Succ n) :~: Succ (plus m n)
plusSuccR Zero _ = Refl
plusSuccR (Succ m) n = congSucc (plusSuccR m n)

castVec :: m :~: n -> Vec a m -> Vec a n
castVec Refl xs = xs

-- | Convert a non-negative Int to a runtime Nat.
intToNat :: Int -> Nat
intToNat n
  | n <= 0 = Zero
  | otherwise = Succ (intToNat (n - 1))

-- | Build an arithmetic progression vector of runtime length n.
iotaVec :: foreach (n :: Nat) -> Int -> Vec Int n
iotaVec Zero _ = VNil
iotaVec (Succ n) start = VCons start (iotaVec n (start + 1))

-- | Pointwise linear combination of two equal-length vectors.
mixVec :: Vec Int n -> Vec Int n -> Vec Int n
mixVec VNil VNil = VNil
mixVec (VCons x xs) (VCons y ys) = VCons (x * 3 + y * 2) (mixVec xs ys)

-- | Strict inner product of two equal-length vectors.
dotVec :: Vec Int n -> Vec Int n -> Int
dotVec = go 0
  where
    go :: Int -> Vec Int m -> Vec Int m -> Int
    go !acc VNil VNil = acc
    go !acc (VCons x xs) (VCons y ys) = go (acc + x * y) xs ys

-- | Safe Fin-indexed vector lookup.
lookupVec :: Fin n -> Vec a n -> a
lookupVec FZ (VCons x _) = x
lookupVec (FS i) (VCons _ xs) = lookupVec i xs

-- | Tail-recursive vector reversal using propositional equality refinements.
revAccVec :: forall a. foreach (m :: Nat) -> foreach (n :: Nat) -> Vec a m -> Vec a n -> Vec a (plus m n)
revAccVec Zero _ VNil acc = acc
revAccVec (Succ m) n (VCons x xs) acc =
  case plusSuccR m n of
    Refl -> revAccVec @a m (Succ n) xs (VCons x acc)

reverseVec :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n
reverseVec n xs =
  case plusZeroR n of
    Refl -> revAccVec @a n Zero xs VNil

-- | Pack an erased-length vector into Exists and fold it back.
packIota :: foreach (n :: Nat) -> Int -> Exists (Vec Int)
packIota n seed =
  let vec :: Vec Int n
      vec = iotaVec n seed
  in Pack (type n) vec

packAndSum :: Int -> Int -> Int
packAndSum len seed =
  withExists (packIota (intToNat len) seed) (\_ vec -> dotVec vec vec)

-- | Intrinsically-typed STLC object language.
data Ty = TInt | TArr Ty Ty
data Ctx = Empty | Snoc Ctx Ty

type Var :: Ctx -> Ty -> Type
data Var g t where
  VZ :: Var (Snoc g t) t
  VS :: Var g t -> Var (Snoc g s) t

type Expr :: Ctx -> Ty -> Type
data Expr g t where
  EVar :: Var g t -> Expr g t
  ELit :: !Int -> Expr g TInt
  EAdd :: Expr g TInt -> Expr g TInt -> Expr g TInt
  ELam :: Expr (Snoc g a) b -> Expr g (TArr a b)
  EApp :: Expr g (TArr a b) -> Expr g a -> Expr g b

type Val :: Ty -> Type
data Val t where
  VInt :: !Int -> Val TInt
  VFun :: (Val a -> Val b) -> Val (TArr a b)

type Env :: Ctx -> Type
data Env g where
  ENil  :: Env Empty
  ECons :: !(Val t) -> !(Env g) -> Env (Snoc g t)

lookupEnv :: Var g t -> Env g -> Val t
lookupEnv VZ (ECons v _) = v
lookupEnv (VS x) (ECons _ env) = lookupEnv x env

eval :: Env g -> Expr g t -> Val t
eval env (EVar x) = lookupEnv x env
eval _ (ELit k) = VInt k
eval env (EAdd a b) =
  case (eval env a, eval env b) of
    (VInt x, VInt y) -> VInt (x + y)
eval env (ELam body) = VFun (\arg -> eval (ECons arg env) body)
eval env (EApp f x) =
  case eval env f of
    VFun k -> k (eval env x)

stlcStep :: Int -> Int
stlcStep seed =
  let term :: Expr Empty TInt
      term =
        EApp
          (ELam (EAdd (EVar VZ) (EAdd (EVar VZ) (ELit 7))))
          (EAdd (ELit seed) (ELit 3))
  in case eval ENil term of
    VInt r -> r

{-# NOINLINE runIteration #-}
runIteration :: foreach (n32 :: Nat) -> Int -> Int
runIteration n32 k =
  let s = k `mod` 17
      xs :: Vec Int n32
      xs = iotaVec n32 s
      ys :: Vec Int n32
      ys = reverseVec @Int n32 xs
      zs :: Vec Int n32
      zs = mixVec xs ys
      tap = case zs of
        VNil -> 0
        VCons x VNil -> x
        VCons a0 (VCons a1 rest) -> lookupVec (FS FZ) (VCons a0 (VCons a1 rest))
      d = dotVec xs zs
      ex = packAndSum 16 s
      ev = stlcStep s
  in d + tap + ex + ev

runLoop :: Int -> Int
runLoop total =
  let n32 = intToNat 32
      go :: Int -> Int -> Int
      go !acc 0 = acc
      go !acc i = go (acc + runIteration n32 i) (i - 1)
  in go 0 total

main :: IO Unit
main = print (runLoop 2000)
