#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -Werror=inaccessible-code -Werror=incomplete-patterns -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
src=$PWD

for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/NativeRetainedCastSourceCallProvider.hs" .
    "${ghc[@]}" "-$opt" -c NativeRetainedCastSourceCallProvider.hs
    cp ./*.hi "$src/NativeRetainedCastSourceCall.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -c -fno-code -fwrite-interface NativeRetainedCastSourceCall.hs
    "${ghc[@]}" "-$opt" -c NativeRetainedCastSourceCall.hs
    "${ghc[@]}" "-$opt" NativeRetainedCastSourceCall.o \
        ../provider/NativeRetainedCastSourceCallProvider.o -o run
    actual=$(./run)
    test "$actual" = "(True,True,True,True)" || {
        printf 'unexpected run output (-%s): %s\n' "$opt" "$actual" >&2
        exit 1
    }
done

mkdir -p "$out/ghci"
cp "$src/NativeRetainedCastSourceCallProvider.hs" "$src/NativeRetainedCastSourceCall.hs" "$out/ghci/"
cd "$out/ghci"
ghci_out=$("$1" "-B$2" --interactive -ignore-dot-ghci -v0 -dcore-lint -package-env - -i -i. NativeRetainedCastSourceCall.hs <<'GHCI'
main
GHCI
)
grep -Fxq "(True,True,True,True)" <<<"$ghci_out" || {
    printf 'unexpected ghci output: %s\n' "$ghci_out" >&2
    exit 1
}

mkdir -p "$out/neg"
cp "$out/O0/provider/"*.hi "$out/neg/"
cd "$out/neg"

# Negative control: mismatched step target must still be rejected.
cat >BadBetaTarget.hs <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module BadBetaTarget where
import NativeRetainedCastSourceCallProvider

badBeta :: forall a.
           foreach (body :: Term (CExt CEmpty a) a)
        -> foreach (m :: Term CEmpty a)
        -> Value m
        -> Progress (App (Lam body) m)
badBeta body m w = PStep m (BetaLam w)
HS
if "${ghc[@]}" -O0 -c BadBetaTarget.hs >bad_beta.log 2>&1 ||
   ! grep -Eq '\[GHC-(83865|05617)\]' bad_beta.log; then
    echo "ERROR: BadBetaTarget check failed" >&2
    cat bad_beta.log >&2
    exit 1
fi
