{-# LANGUAGE DependentTypes, LambdaCase, ListTuplePuns #-}
module Main (main) where

import Data.Type.Equality ((:~:) (Refl))
import Data.Nat.Dependent (Nat(..), eqSize, natToInt, plus)
import Data.Nat.Dependent.Properties (eqSizeRefl, plusZero)
import Prelude (Bool(..), Eq(..), IO, error, pure)

data Ty = TNat | TArr Ty Ty
data Ctx = CEmpty | CExt Ctx Ty
data Lookup (g :: Ctx) (a :: Ty) where
  Z :: Lookup (CExt g a) a
  S :: Lookup g a -> Lookup (CExt g b) a
data Term (g :: Ctx) (a :: Ty) where
  Var :: Lookup g a -> Term g a
  Lam :: Term (CExt g a) b -> Term g (TArr a b)

ext :: (forall a. Lookup g a -> Lookup d a) -> Lookup (CExt g b) a -> Lookup (CExt d b) a
ext _ Z     = Z
ext r (S x) = S (r x)

rename :: (forall a. Lookup g a -> Lookup d a) -> Term g a -> Term d a
rename r (Var x) = Var (r x)
rename r (Lam n) = Lam (rename (ext r) n)

exts :: (forall a. Lookup g a -> Term d a) -> Lookup (CExt g b) a -> Term (CExt d b) a
exts _ Z     = Var Z
exts s (S x) = rename S (s x)

subst :: (forall a. Lookup g a -> Term d a) -> Term g a -> Term d a
subst s (Var x) = s x
subst s (Lam n) = Lam (subst (exts s) n)

substSingle :: Term (CExt g b) a -> Term g b -> Term g a
substSingle m v = subst (\case { Z -> v; S x -> Var x }) m

substLaw :: forall g a (v :: Term g a). substSingle (Var Z) v :~: v
substLaw = Refl

checkNat :: foreach (n :: Nat) -> (plus n Zero :~: n, eqSize n n :~: True)
checkNat n = (plusZero n, eqSizeRefl n)

main :: IO ()
main = do
  let n2 = Succ (Succ Zero)
  case (checkNat n2, substLaw @CEmpty @(TArr TNat TNat) @(Lam (Var Z))) of
    ((Refl, Refl), Refl) ->
      if natToInt (plus n2 Zero) == 2
        then pure ()
        else error "DepProg mismatch"
