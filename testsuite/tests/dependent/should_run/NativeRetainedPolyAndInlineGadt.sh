#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -Werror=inaccessible-code -Werror=incomplete-patterns -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
src=$PWD

for ql in -XNoImpredicativeTypes -XImpredicativeTypes; do
    for opt in O0 O2; do
        mkdir -p "$out/$ql/$opt"/{provider,client}
        cd "$out/$ql/$opt/provider"
        cp "$src/NativeRetainedPolyAndInlineGadtProvider.hs" .
        "${ghc[@]}" "$ql" "-$opt" -c NativeRetainedPolyAndInlineGadtProvider.hs
        cp ./*.hi "$src/NativeRetainedPolyAndInlineGadt.hs" ../client/
        cd ../client
        "${ghc[@]}" "$ql" "-$opt" -c -fno-code -fwrite-interface NativeRetainedPolyAndInlineGadt.hs
        "${ghc[@]}" "$ql" "-$opt" -c NativeRetainedPolyAndInlineGadt.hs
        "${ghc[@]}" "$ql" "-$opt" NativeRetainedPolyAndInlineGadt.o \
            ../provider/NativeRetainedPolyAndInlineGadtProvider.o -o run
        actual=$(./run)
        test "$actual" = "(True,True,True,True,True,True,True)" || {
            printf 'unexpected run output (%s -%s): %s\n' "$ql" "$opt" "$actual" >&2
            exit 1
        }
    done
done

mkdir -p "$out/ghci"
cp "$src/NativeRetainedPolyAndInlineGadtProvider.hs" "$src/NativeRetainedPolyAndInlineGadt.hs" "$out/ghci/"
cd "$out/ghci"
ghci_out=$("$1" "-B$2" --interactive -ignore-dot-ghci -v0 -package-env - -i -i. NativeRetainedPolyAndInlineGadt.hs <<'GHCI'
main
GHCI
)
grep -Fxq "(True,True,True,True,True,True,True)" <<<"$ghci_out" || {
    printf 'unexpected ghci output: %s\n' "$ghci_out" >&2
    exit 1
}

mkdir -p "$out/neg"
cp "$out/-XNoImpredicativeTypes/O0/provider/"*.hi "$out/neg/"
cd "$out/neg"

# 1. Wrong inline GADT signature where UCtx Zero Zero is expected must fail.
cat >WrongAnnotDomain.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeApplications, TypeOperators #-}
module WrongAnnotDomain where
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeRetainedPolyAndInlineGadtProvider as P

plugHoleId :: forall (t :: P.UTerm 'P.Zero).
              foreach (c :: P.UCtx 'P.Zero 'P.Zero) ->
              (c ~ 'P.CHole) =>
              P.plug c t :~: t
plugHoleId _ = Refl

bad = plugHoleId @(P.ULam (P.UVar P.FZ) :: P.UTerm 'P.Zero)
                 (P.CHole :: P.UCtx 'P.Zero ('P.Succ 'P.Zero))
HS
if "${ghc[@]}" -O0 -c WrongAnnotDomain.hs >wrong_dom.log 2>&1 ||
   ! grep -Eq '\[GHC-(83605|83865)\]' wrong_dom.log; then
    echo "ERROR: WrongAnnotDomain check failed" >&2
    cat wrong_dom.log >&2
    exit 1
fi

# 2. Constructor incompatible with its own written signature must fail even when signature matches binder.
cat >WrongAnnotCtor.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module WrongAnnotCtor where
import qualified NativeRetainedPolyAndInlineGadtProvider as P

acceptNonDiag :: foreach (c :: P.UCtx 'P.Zero ('P.Succ 'P.Zero)) -> ()
acceptNonDiag _ = ()

bad :: ()
bad = acceptNonDiag (P.CHole :: P.UCtx 'P.Zero ('P.Succ 'P.Zero))
HS
if "${ghc[@]}" -O0 -c WrongAnnotCtor.hs >wrong_ctor.log 2>&1 ||
   ! grep -Eq '\[GHC-(83605|83865)\]' wrong_ctor.log; then
    echo "ERROR: WrongAnnotCtor check failed" >&2
    cat wrong_ctor.log >&2
    exit 1
fi

# 3. Genuine GADT local-equality untouchability under a retained call must still be rejected.
cat >UntouchableGadtLocalEq.hs <<'HS'
{-# LANGUAGE DataKinds, ExistentialQuantification, ExperimentalDependentTypes, GADTs #-}
module UntouchableGadtLocalEq where
import Data.Kind (Type)
import qualified NativeRetainedPolyAndInlineGadtProvider as P

data G :: Type -> Type where
  GInt :: Int -> G Int

data Hidden = forall t. Hidden (G t) (G t)

useRetained :: foreach (n :: P.Nat) -> a -> a
useRetained _ x = x

escapeUntouchable h =
  useRetained P.Zero (case (h :: Hidden) of Hidden (GInt _) a -> a)
HS
if "${ghc[@]}" -O0 -c UntouchableGadtLocalEq.hs >untouch.log 2>&1 ||
   ! grep -Eq '\[GHC-(25897|83865)\]' untouch.log; then
    echo "ERROR: UntouchableGadtLocalEq check failed" >&2
    cat untouch.log >&2
    exit 1
fi
