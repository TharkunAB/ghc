{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Type.Equality ((:~:) (Refl))
import qualified Prelude

-- | Peano natural numbers.
data Nat = Zero | Succ Nat
    deriving (Prelude.Eq, Prelude.Show)

-- | Length-indexed vectors.
data Vec a (n :: Nat) where
    VNil  :: Vec a 'Zero
    VCons :: a -> Vec a n -> Vec a ('Succ n)

deriving instance Prelude.Eq a => Prelude.Eq (Vec a n)
deriving instance Prelude.Show a => Prelude.Show (Vec a n)

-- | Bounded natural positions.
data Fin (n :: Nat) where
    FZ :: Fin ('Succ n)
    FS :: Fin n -> Fin ('Succ n)

-- | Direct structurally recursive addition.
plus :: Nat -> Nat -> Nat
plus Zero     n = n
plus (Succ m) n = Succ (plus m n)

-- | Direct structurally recursive vector append.
appendVec :: Vec a n -> Vec a m -> Vec a (plus n m)
appendVec VNil         ys = ys
appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

-- | Direct structurally recursive bounded vector lookup (GADT-exhaustive).
lookupVec :: Fin n -> Vec a n -> a
lookupVec FZ     (VCons x _)  = x
lookupVec (FS i) (VCons _ xs) = lookupVec i xs

-- | Signed non-recursive congruence helper.
congSucc :: m :~: n -> 'Succ m :~: 'Succ n
congSucc Refl = Refl

-- | Direct structurally recursive associativity proof calling signed 'congSucc'.
plusAssoc :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
          -> plus (plus m n) p :~: plus m (plus n p)
plusAssoc Zero     _ _ = Refl
plusAssoc (Succ m) n p = congSucc (plusAssoc m n p)

-- | Signed total helper for call-graph edge verification.
stepHelper :: Nat -> Nat
stepHelper Zero     = Succ Zero
stepHelper (Succ n) = Succ (Succ n)

-- | Structurally recursive caller that depends on signed 'stepHelper'.
callsCheckedHelper :: Nat -> Nat
callsCheckedHelper Zero     = Zero
callsCheckedHelper (Succ n) = plus (stepHelper n) (callsCheckedHelper n)

-- | Signed divergent helper for negative call-graph propagation.
divergentHelper :: Nat -> Nat
divergentHelper n = divergentHelper n

-- | Caller whose signed callee is not structurally total.
callsDivergentHelper :: Nat -> Nat
callsDivergentHelper Zero     = Zero
callsDivergentHelper (Succ n) = divergentHelper n

-- | Non-structural self-recursion.
loop :: a -> a
loop x = loop x

-- | Higher-order callback application.
apply :: (a -> b) -> a -> b
apply f x = f x

-- | Partial pattern match missing the empty list equation.
headOnly :: [a] -> a
headOnly (x : _) = x

-- | Call to an unverified ordinary imported function.
revExternal :: [a] -> [a]
revExternal xs = Prelude.reverse xs

-- | Mutually recursive parity functions.
evenN :: Nat -> Prelude.Bool
evenN Zero = Prelude.True
evenN (Succ n) = oddN n

oddN :: Nat -> Prelude.Bool
oddN Zero = Prelude.False
oddN (Succ n) = evenN n

main :: Prelude.IO ()
main = do
    let v1 = VCons 'a' (VCons 'b' VNil)
        v2 = VCons 'c' VNil
        v3 = appendVec v1 v2
        ok = plus (Succ Zero) (Succ (Succ Zero)) Prelude.== Succ (Succ (Succ Zero))
             Prelude.&& lookupVec (FS FZ) v3 Prelude.== 'b'
             Prelude.&& lookupVec (FS (FS FZ)) v3 Prelude.== 'c'
             Prelude.&& callsCheckedHelper (Succ (Succ Zero)) Prelude.== Succ (Succ (Succ Zero))
             Prelude.&& apply (\n -> Succ n) Zero Prelude.== Succ Zero
             Prelude.&& headOnly ['q'] Prelude.== 'q'
             Prelude.&& revExternal ['x', 'y'] Prelude.== ['y', 'x']
             Prelude.&& evenN (Succ (Succ Zero))
             Prelude.&& case plusAssoc (Succ Zero) (Succ Zero) Zero of Refl -> Prelude.True
    if ok
        then Prelude.putStrLn "totality-advisory-ok"
        else Prelude.ioError (Prelude.userError "totality runtime check failed")
