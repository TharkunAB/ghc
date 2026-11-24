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
