#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

ghc=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint -fdiagnostics-color=never)
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

# 1. Positive matrix: forward/reversed order, prenex/rank-2 ext, named/constructor weakening,
#    -O0/-O2, native / -fno-code / -fwrite-if-simplified-core, and source-hidden .hi client!
for opt in -O0 -O2; do
  for order in "" "-DREVERSED_ORDER" "-DPRENEX_EXT -DDIRECT_CON_WEAKEN"; do
    read -ra order_flags <<<"$order"
    for mode in native nocode simpcore; do
      tag="${order//[^A-Za-z]/_}"
      dir="$out/$opt/${tag:-default}/$mode"
      mkdir -p "$dir/provider" "$dir/client"
      cp "$src/NativeSameModuleClosurePlug.hs" "$dir/provider/"
      extra=()
      case "$mode" in
        nocode)   extra=(-fno-code -fwrite-interface) ;;
        simpcore) extra=(-fwrite-if-simplified-core) ;;
      esac
      (cd "$dir/provider" && "${ghc[@]}" "$opt" "${extra[@]}" "${order_flags[@]}" -c NativeSameModuleClosurePlug.hs)
      "${ghc[@]}" --show-iface "$dir/provider/NativeSameModuleClosurePlug.hi" > "$dir/provider.iface"
      grep -Fq 'SourceTable T:plug' "$dir/provider.iface"
      grep -Fq 'SourceTable T:plugS' "$dir/provider.iface"
      grep -Fq 'SourceTable T:extU' "$dir/provider.iface"
      grep -Fq 'CheckedClosureSourceTable T:renameU [extU]' "$dir/provider.iface"
      grep -Fq 'CheckedClosureSourceTable T:substSingleU' "$dir/provider.iface"
      grep -Fq 'CheckedClosureSourceTable T:substSingle' "$dir/provider.iface"
      grep -Fq 'CheckedClosureSourceTable T:substDouble' "$dir/provider.iface"

      cp "$dir/provider/NativeSameModuleClosurePlug.hi" "$dir/client/"
      cat > "$dir/client/Client.hs" <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, TypeApplications, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:) (Refl))
import NativeSameModuleClosurePlug

checkStep :: Step m n -> String
checkStep (BetaLam _)   = "lam"
checkStep (BetaLst _ _) = "lst"

main :: IO ()
main = do
  let p1 = plugLamEq @('Succ 'Zero) @'Zero @'CHole @('UVar 'FZ)
      p2 = betaId @'CEmpty @('TArr 'TNat 'TNat) @('Lam ('Var 'Z))
      p3 = betaUntypedEq @('Succ 'Zero) @'FZ
  case (p1, p2, p3) of
    (Refl, Refl, Refl) -> print (checkStep betaShift, checkStep betaDouble)
HS
      if [[ "$mode" == "nocode" ]]; then
        (cd "$dir/client" && "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c Client.hs)
      else
        (
          cd "$dir/client"
          "${ghc[@]}" "$opt" -c Client.hs
          "${ghc[@]}" "$opt" Client.o ../provider/NativeSameModuleClosurePlug.o -o run
          test "$(./run)" = '("lam","lst")'
        )
      fi
    done
  done
done

# 2.Unused speculative closure/traversal before TH splice with abstract datatypes:
#   must compile cleanly AND prune T:plug / T:substSingle / $closure: from .hi!
mkdir -p "$out/pruned"
cat > "$out/pruned/PrunedBeforeSplice.hs" <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase #-}
{-# LANGUAGE PolyKinds, RankNTypes, TemplateHaskell #-}
module PrunedBeforeSplice (Nat, UCtx, UTerm, Ty, Ctx, Lookup, Term, plug, substSingle) where

data Nat = Zero | Succ Nat
data Fin (n :: Nat) where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)
data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm ('Succ n) -> UTerm n
data UCtx (m :: Nat) (n :: Nat) where
  CHole :: UCtx n n
  CLam  :: UCtx m ('Succ n) -> UCtx m n

plug :: UCtx m n -> UTerm m -> UTerm n
plug CHole    t = t
plug (CLam c) t = ULam (plug c t)

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

$(pure [])

ok :: Int
ok = 1
HS
for opt in -O0 -O2; do
  (cd "$out/pruned" && "${ghc[@]}" "$opt" -fforce-recomp -c PrunedBeforeSplice.hs)
  "${ghc[@]}" --show-iface "$out/pruned/PrunedBeforeSplice.hi" > "$out/pruned/pruned.iface"
  if grep -Eq 'T:plug|T:substSingle|\$closure:' "$out/pruned/pruned.iface"; then
    cat "$out/pruned/pruned.iface" >&2
    exit 1
  fi
done

# 3. Strict negative controls
mkdir -p "$out/neg"
cp "$src/NativeSameModuleClosurePlug.hs" "$out/neg/"
(
  cd "$out/neg"
  # 3a. Hidden constructors / hidden owners / hidden helpers -> [GHC-70566]
  for flag_expected in \
      "HIDE_UCTX_CON:Export the constructors of UCtx" \
      "HIDE_TERM_CON:Export the constructors of Term" \
      "HIDE_LOOKUP_CON:Export the constructors of Lookup" \
      "HIDE_PLUG:Export plug" \
      "HIDE_SUBST_SINGLE:Export substSingle" \
      "HIDE_EXTS:Export exts"; do
    flag="${flag_expected%%:*}"
    expected="${flag_expected#*:}"
    if "${ghc[@]}" -O0 -fforce-recomp "-D$flag" -c NativeSameModuleClosurePlug.hs >neg.log 2>&1 ||
       ! grep -Fq '[GHC-70566]' neg.log ||
       ! grep -Fq "$expected" neg.log; then
      echo "Failed negative control $flag" >&2
      cat neg.log >&2
      exit 1
    fi
  done

  # 3b. Local RULES -> [GHC-83605]
  for flag in LOCAL_RULE_PLUG LOCAL_RULE_SUBST LOCAL_RULE_HELPER; do
    if "${ghc[@]}" -O2 -fforce-recomp -Wno-inline-rule-shadowing "-D$flag" -c NativeSameModuleClosurePlug.hs >neg.log 2>&1 ||
       ! grep -Fq '[GHC-83605]' neg.log; then
      echo "Failed local rule control $flag" >&2
      cat neg.log >&2
      exit 1
    fi
  done

  # 3c. SPECIALISE, -fhpc, strict/wrong bodies -> [GHC-68947] / [GHC-70566]
  for flag in SPECIALISE_PLUG SPECIALISE_SUBST STRICT_PLUG STRICT_SUBST WRONG_SUBST; do
    if "${ghc[@]}" -O2 -fforce-recomp "-D$flag" -c NativeSameModuleClosurePlug.hs >neg.log 2>&1 ||
       ! grep -Fq '[GHC-68947]' neg.log; then
      echo "Failed body/specialise control $flag" >&2
      cat neg.log >&2
      exit 1
    fi
  done
  if "${ghc[@]}" -O2 -fforce-recomp -DWRONG_PLUG -c NativeSameModuleClosurePlug.hs >neg.log 2>&1 ||
     ! grep -Fq '[GHC-70566]' neg.log; then
    echo "Failed body control WRONG_PLUG" >&2
    cat neg.log >&2
    exit 1
  fi
  for extra in "" "-fno-code -fwrite-interface"; do
    read -ra ex_flags <<<"$extra"
    if "${ghc[@]}" -O0 -fforce-recomp -fhpc "${ex_flags[@]}" -c NativeSameModuleClosurePlug.hs >hpc.log 2>&1 ||
       ! grep -Fq '[GHC-68947]' hpc.log; then
      cat hpc.log >&2
      exit 1
    fi
  done

  # 3d. Forged capture target -> [GHC-83865]
  if "${ghc[@]}" -O0 -fforce-recomp -DBAD_CAPTURE_TARGET -c NativeSameModuleClosurePlug.hs >cap.log 2>&1 ||
     ! grep -Fq '[GHC-83865]' cap.log; then
    cat cap.log >&2
    exit 1
  fi
)

# 4. Imported orphan RULES via .hs-boot must be rejected even under -fno-code -fomit-interface-pragmas!
mkdir -p "$out/orphan"
(
  cd "$out/orphan"
  cat > SameModSub.hs-boot <<'HS'
{-# LANGUAGE DataKinds, GADTs, PolyKinds #-}
module SameModSub (Ty(..), Ctx(..), Lookup(..), Term(..), substSingle) where
data Ty = TNat | TArr Ty Ty
data Ctx = CEmpty | CExt Ctx Ty
data Lookup (g :: Ctx) (a :: Ty) where
  Z :: Lookup (CExt g a) a
  S :: Lookup g a -> Lookup (CExt g b) a
data Term (g :: Ctx) (a :: Ty) where
  Var :: Lookup g a -> Term g a
  Lam :: Term (CExt g a) b -> Term g (TArr a b)
substSingle :: Term (CExt g b) a -> Term g b -> Term g a
HS
  cat > OrphanRule.hs <<'HS'
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}
module OrphanRule () where
import {-# SOURCE #-} SameModSub (Ty(..), Ctx(..), Term, substSingle)
{-# RULES "orphan/substSingle" [~1] forall (m :: Term ('CExt g 'TNat) 'TNat) (v :: Term g 'TNat). substSingle m v = v #-}
HS
  cat > SameModSub.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase, PolyKinds, RankNTypes, TypeOperators #-}
module SameModSub (Ty(..), Ctx(..), Lookup(..), Term(..), ext, rename, exts, subst, substSingle, law) where
import Data.Type.Equality ((:~:) (Refl))
import OrphanRule ()

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

{-# NOINLINE substSingle #-}
substSingle :: Term (CExt g b) a -> Term g b -> Term g a
substSingle m v = subst (\case { Z -> v; S x -> Var x }) m

law :: forall g a (v :: Term g a). substSingle (Var Z) v :~: v
law = Refl
HS
  for opt in -O0 -O2; do
    for extra in "" "-fno-code -fwrite-interface -fomit-interface-pragmas"; do
      read -ra ex_flags <<<"$extra"
      rm -f ./*.hi ./*.o ./*.hi-boot ./*.o-boot
      "${ghc[@]}" "$opt" "${ex_flags[@]}" -c SameModSub.hs-boot
      "${ghc[@]}" "$opt" "${ex_flags[@]}" -c OrphanRule.hs
      if "${ghc[@]}" "$opt" "${ex_flags[@]}" -c SameModSub.hs >orphan.log 2>&1 ||
         ! grep -Fq '[GHC-83605]' orphan.log; then
        cat orphan.log >&2
        exit 1
      fi
    done
  done
)

# 5. Interactive GHCi check
mkdir -p "$out/ghci"
"${ghc[@]}" -XDataKinds -XTypeApplications --interactive -ignore-dot-ghci \
  -odir "$out/ghci" -hidir "$out/ghci" -i -i"$src" \
  "$src/NativeSameModuleClosurePlug.hs" \
  -e 'case (plugLamEq @(Succ Zero) @Zero @CHole @(UVar FZ), betaId @CEmpty @(TArr TNat TNat) @(Lam (Var Z))) of (r1, r2) -> (r1, r2)' \
  > "$out/ghci.out"
grep -Fxq '(Refl,Refl)' "$out/ghci.out"

# 6. Same-module named singleSubst/substSingle + untyped substU/extsU + sigma combinators
mkdir -p "$out/same-sigma"
(
  cd "$out/same-sigma"
  cat > SameModNamedAndSigma.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, CPP, DependentTypes, LambdaCase #-}
module SameModNamedAndSigma
#ifdef HIDE_SINGLE_SUBST
  ( Ty(..), Ctx(..), Lookup(..), Term(..), weakenLookup, ext, rename, exts, subst, substSingle, Step(..) )
#endif
  where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Exts (U)
import GHC.Types (Matchability)

data Ty = TNat | TArr Ty Ty
data Ctx = CEmpty | CExt Ctx Ty
data Lookup (g :: Ctx) (a :: Ty) where
  Z :: Lookup (CExt g a) a
  S :: Lookup g a -> Lookup (CExt g b) a
data Term (g :: Ctx) (a :: Ty) where
  Var  :: Lookup g a -> Term g a
  Lam  :: Term (CExt g a) b -> Term g (TArr a b)
  App  :: Term g (TArr a b) -> Term g a -> Term g b
  Zero :: Term g TNat

weakenLookup :: forall d b a. Lookup d a -> Lookup (CExt d b) a
weakenLookup x = S x

ext :: (forall a. Lookup g a -> Lookup d a) -> (forall a. Lookup (CExt g b) a -> Lookup (CExt d b) a)
ext _ Z     = Z
ext r (S x) = S (r x)

rename :: (forall a. Lookup g a -> Lookup d a) -> Term g b -> Term d b
rename r (Var x)   = Var (r x)
rename r (Lam n)   = Lam (rename (ext r) n)
rename r (App l m) = App (rename r l) (rename r m)
rename _ Zero      = Zero

exts :: forall g d b. (forall a. Lookup g a -> Term d a) -> (forall a. Lookup (CExt g b) a -> Term (CExt d b) a)
exts _ Z     = Var Z
exts s (S x) = rename weakenLookup (s x)

subst :: (forall a. Lookup g a -> Term d a) -> Term g b -> Term d b
subst s (Var x)   = s x
subst s (Lam n)   = Lam (subst (exts s) n)
subst s (App l m) = App (subst s l) (subst s m)
subst _ Zero      = Zero

singleSubst :: forall g a. Term g a -> (forall b. Lookup (CExt g a) b -> Term g b)
singleSubst w Z     = w
singleSubst _ (S x) = Var x

substSingle :: Term (CExt g a) b -> Term g a -> Term g b
substSingle n w = subst (singleSubst w) n

#ifdef RULE_SINGLE_SUBST
{-# RULES "hijackSingleSubst" forall w. singleSubst w Z = Zero #-}
#endif

type Step :: forall (g :: Ctx) (a :: Ty). Term g a -> Term g a -> Type
data Step m n where
  BetaLam :: forall g a b (body :: Term (CExt g a) b) (v :: Term g a).
             Step (App (Lam body) v) (substSingle body v)

sameModTypedEq :: forall g a b (n :: Term (CExt g a) b) (v :: Term g a).
                  substSingle n v :~: subst (singleSubst v) n
sameModTypedEq = Refl

#ifndef HIDE_SINGLE_SUBST
data Nat = NZero | NSucc Nat
data Fin (n :: Nat) where
  FZ :: Fin (NSucc n)
  FS :: Fin n -> Fin (NSucc n)
data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm (NSucc n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n

extU :: (Fin m -> Fin n) -> Fin (NSucc m) -> Fin (NSucc n)
extU _   FZ     = FZ
extU rho (FS x) = FS (rho x)

renameU :: (Fin m -> Fin n) -> UTerm m -> UTerm n
renameU rho (UVar x)   = UVar (rho x)
renameU rho (ULam b)   = ULam (renameU (extU rho) b)
renameU rho (UApp l m) = UApp (renameU rho l) (renameU rho m)

extsU :: (Fin m -> UTerm n) -> Fin (NSucc m) -> UTerm (NSucc n)
extsU _     FZ     = UVar FZ
extsU sigma (FS x) = renameU shift (sigma x)

substU :: (Fin m -> UTerm n) -> UTerm m -> UTerm n
substU sigma (UVar x)   = sigma x
substU sigma (ULam b)   = ULam (substU (extsU sigma) b)
substU sigma (UApp l m) = UApp (substU sigma l) (substU sigma m)

substSingleInlineU :: UTerm (NSucc n) -> UTerm n -> UTerm n
substSingleInlineU b w = substU (\case { FZ -> w; FS x -> UVar x }) b

singleSubstU :: UTerm n -> Fin (NSucc n) -> UTerm n
singleSubstU w FZ     = w
singleSubstU _ (FS x) = UVar x

substSingleNamedU :: UTerm (NSucc n) -> UTerm n -> UTerm n
substSingleNamedU b w = substU (singleSubstU w) b

type Rename m n = Fin m -> Fin n
type Subst m n = Fin m -> UTerm n

ids :: Subst m m
ids = UVar

shift :: Rename m (NSucc m)
shift = FS

myShift :: Rename m (NSucc m)
myShift = FS

ren :: Rename m n -> Subst m n
ren r x = UVar (r x)

consSubst :: UTerm n -> Subst m n -> Subst (NSucc m) n
consSubst t _ FZ     = t
consSubst _ s (FS x) = s x

seqSubst :: Subst m n -> Subst n k -> Subst m k
seqSubst p q x = substU q (p x)

cong :: forall {a} {b} {m :: Matchability}.
        forall (f :: a %m -> b) -> forall (x :: a) (y :: a).
        x :~: y -> f x :~: f y
cong _ Refl = Refl

sameIds :: forall m (x :: Fin m). substU (ids @m) (UVar x) :~: UVar x
sameIds = Refl

sameShiftSig :: forall m (x :: Fin m). renameU (shift @m) (UVar x) :~: UVar (FS x)
sameShiftSig = Refl

sameShiftCong :: forall m n (p :: Fin m %U -> UTerm n) (q :: Fin m %U -> UTerm n) (x :: Fin m).
                 (p x :~: q x) -> extsU p (FS x) :~: extsU q (FS x)
sameShiftCong eq = cong (renameU shift) eq

t2tOnlyCong :: forall n (x :: Fin n). UVar (FS x) :~: UVar (FS x)
t2tOnlyCong = cong (renameU myShift) (Refl :: UVar x :~: UVar x)

sameRen :: forall m n (r :: Fin m %U -> Fin n) (x :: Fin m).
           substU (ren r) (UVar x) :~: UVar (r x)
sameRen = Refl

sameConsZ :: forall m n (t :: UTerm n) (s :: Fin m %U -> UTerm n).
             consSubst t s FZ :~: t
sameConsZ = Refl

sameSeq :: forall m n k (p :: Fin m %U -> UTerm n) (q :: Fin n %U -> UTerm k) (x :: Fin m).
           substU (seqSubst p q) (UVar x) :~: substU q (p x)
sameSeq = Refl

sameNamedU :: forall n (b :: UTerm (NSucc n)) (w :: UTerm n).
              substSingleNamedU b w :~: substU (singleSubstU w) b
sameNamedU = Refl

sameInlineUZ :: forall n (w :: UTerm n). substSingleInlineU (UVar FZ) w :~: w
sameInlineUZ = Refl
#endif
HS
  for opt in -O0 -O2; do
    "${ghc[@]}" "$opt" -fforce-recomp -c SameModNamedAndSigma.hs
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -fforce-recomp -c SameModNamedAndSigma.hs
  done
  if "${ghc[@]}" -O0 -fforce-recomp -DHIDE_SINGLE_SUBST -c SameModNamedAndSigma.hs >hide.log 2>&1 ||
     ! grep -Fq '[GHC-70566]' hide.log; then
    cat hide.log >&2
    exit 1
  fi
  if "${ghc[@]}" -O2 -fforce-recomp -Wno-inline-rule-shadowing -DRULE_SINGLE_SUBST \
       -c SameModNamedAndSigma.hs >srule.log 2>&1 ||
     ! grep -Fq '[GHC-83605]' srule.log; then
    cat srule.log >&2
    exit 1
  fi
)
