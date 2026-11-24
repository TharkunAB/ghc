{-# LANGUAGE DependentTypes, UnicodeSyntax #-}
module Main where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import GHC.Tuple (Unit)
import GHC.Types (List)
import NativeProposal378SyntaxAndFlagsProvider

-- Unicode ∏ + shared-type multi-binder group (m n p :: Nat):
plusAssoc :: ∏ (m n p :: Nat) -> plus (plus m n) p :~: plus m (plus n p)
plusAssoc Zero _ _ = Refl
plusAssoc (Succ m) n p = case plusAssoc m n p of
  Refl -> Refl

-- Shared-type multi-binder groups in forall (both SpecifiedSpec and InferredSpec):
symTrans :: forall {k} (a b c :: k). a :~: b -> b :~: c -> a :~: c
symTrans Refl Refl = Refl

-- Braced specificity binders in invisible foreach:
replicateVec :: foreach (k :: Nat) -> a -> Vec a k
replicateVec Zero _ = VNil
replicateVec (Succ k) x = VCons x (replicateVec k x)

invisOne :: foreach {k :: Nat}. Vec Int k -> Nat
invisOne _ = k

invisTwo :: foreach {m n :: Nat}. Vec Int m -> Vec Int n -> Nat
invisTwo _ _ = plus m n

checkInvis :: ∏ (m n :: Nat) -> Vec Int m -> Vec Int n -> Pair2 Nat Nat
checkInvis _ _ vm vn = MkPair2 (invisOne vm) (invisTwo vm vn)

-- EmptyCase + TypeAbstractions (@a) implied by DependentTypes:
data Void
absurdVoid :: forall a. Void -> a
absurdVoid @a v = case v of {}

-- Multi-binder some values ('.' and '|'):
mkPairVecs :: PairVecs Int
mkPairVecs = Pair (Succ (Succ Zero)) (Pair Zero (MkPair2 (VCons 42 (VCons 43 VNil)) VNil))

mkThreeVecs :: ThreeVecs Int
mkThreeVecs = Pair Zero (Pair (Succ Zero) (Pair Zero (MkTriple3 VNil (VCons 7 VNil) VNil)))

mkPipeVecs :: PipeSomeVecs Int
mkPipeVecs = Pair (Succ Zero) (Pair (Succ Zero) (MkPair2 (VCons 1 VNil) (VCons 2 VNil)))

-- foreach, ∏, and exists in required type argument position (infixexp2) without 'type':
witnessType :: forall (t :: Type) -> List Char
witnessType _ = "rta"

rtaForEach :: List Char
rtaForEach = witnessType (foreach (n :: Nat) -> Vec Int n)

rtaUnicodePi :: List Char
rtaUnicodePi = witnessType (∏ (m n :: Nat) -> Vec Int (plus m n))

rtaExists :: List Char
rtaExists = witnessType (exists {k} (x :: k) (m n :: Nat). Pair2 (Proxy x) (Vec Int m))

-- Cross-module open closure equation without -fwrite-source-equations:
closureEq :: forall g a b (s :: Slot g a).
             replace (Bind @(Push g a) @b (At (Old New))) (At s) :~: Bind (At (Old s))
closureEq = Refl

main :: IO Unit
main = do
  let Refl = plusAssoc (Succ Zero) (Succ Zero) Zero
      Refl = symTrans Refl (plusAssoc (Succ Zero) Zero Zero)
      Refl = closureEq @(Push Empty SNat) @SNat @SNat @New
  case mkPairVecs of
    Pair m r1 -> case r1 of
      Pair n _ -> case mkThreeVecs of
        Pair a r2 -> case r2 of
          Pair b r3 -> case r3 of
            Pair c _ -> case mkPipeVecs of
              Pair p1 r4 -> case r4 of
                Pair p2 _ ->
                  let MkPair2 i1 i2 = checkInvis m p1 (replicateVec m 10) (replicateVec p1 30)
                  in print ( i1
                           , i2
                           , (m, n, a, b, c, p1, p2)
                           , (rtaForEach, rtaUnicodePi, rtaExists)
                           )
