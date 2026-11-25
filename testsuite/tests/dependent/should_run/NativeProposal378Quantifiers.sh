#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
trap 'echo "NativeProposal378Quantifiers.sh failed at line $LINENO: $BASH_COMMAND" >&2' ERR

inputs=$PWD
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

expected='(("aa","b","cc",Succ (Succ Zero),Succ Zero,(Succ Zero,Succ (Succ Zero)),Succ (Succ Zero),Succ (Succ Zero),Succ Zero,Succ (Succ Zero),7,"kkk"),(99,Succ (Succ Zero),Succ (Succ (Succ Zero)),Succ (Succ Zero),42),(42,Just '\''z'\'',7,19,Just '\''m'\'',Succ Zero,(12,"Int"),True,Zero),(1,"refl",1))'

for opt in -O0 -O2; do
    dir="$work/$opt"
    mkdir -p "$dir"/{native,readback,neg}

    # 1. Compile Provider + Client and run binary
    cd "$dir/native"
    cp "$inputs/NativeProposal378QuantifiersProvider.hs" \
       "$inputs/NativeProposal378Quantifiers.hs" .
    "${ghc[@]}" "$opt" -c NativeProposal378QuantifiersProvider.hs
    "${ghc[@]}" --show-iface NativeProposal378QuantifiersProvider.hi >provider.iface

    "${ghc[@]}" "$opt" -c NativeProposal378Quantifiers.hs
    "${ghc[@]}" --show-iface NativeProposal378Quantifiers.hi >client.iface
    "${ghc[@]}" "$opt" NativeProposal378QuantifiersProvider.o \
        NativeProposal378Quantifiers.o -o run
    actual=$(./run)
    [[ "$actual" == "$expected" ]]

    # 2. Separate .hi readback check without Provider.hs source
    cd "$dir/readback"
    cp "$dir/native/NativeProposal378QuantifiersProvider.hi" \
       "$inputs/NativeProposal378Quantifiers.hs" .
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c NativeProposal378Quantifiers.hs

    # 3. Interface matchability audit: no @Matchable on function-kinded forall binders
    python3 - "$dir/native/provider.iface" "$dir/native/client.iface" <<'PY'
import re, sys
pat = re.compile(r"\([A-Za-z0-9_']+ :: [^()]*->[^()]*\)\.\s*@Matchable\b")
for p in sys.argv[1:]:
    txt = open(p, encoding="utf-8", errors="replace").read()
    m = pat.search(txt)
    if m:
        sys.stderr.write(f"Matchable function binder in {p}: {m.group(0)}\n")
        sys.exit(1)
PY
done

# 4. GHCi interactive execution check
mkdir -p "$work/ghci"
cd "$work/ghci"
cp "$inputs/NativeProposal378QuantifiersProvider.hs" \
   "$inputs/NativeProposal378Quantifiers.hs" .
"${ghc[@]}" --interactive -ignore-dot-ghci NativeProposal378Quantifiers.hs -e 'main' >ghci.out
diff -u <(printf '%s\n' "$expected") ghci.out

# 5. Negative controls
cd "$work/-O0/neg"
cp "$work/-O0/native/NativeProposal378QuantifiersProvider.hi" .

# 5a. @arg must be rejected on InferredSpec foreach {k :: Nat}. (both local and imported .hi)
cat >BadInferredAppImported.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module BadInferredAppImported where
import NativeProposal378QuantifiersProvider (Nat(..), Vec(..), onlyInferred)
bad :: Nat
bad = onlyInferred @Char @Zero VNil
HS
if "${ghc[@]}" -O0 -fno-code -c BadInferredAppImported.hs >bad1.log 2>&1; then
    cat bad1.log >&2
    exit 1
fi

cat >BadInferredAppLocal.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module BadInferredAppLocal where
import NativeProposal378QuantifiersProvider (Nat(..), Vec(..))
localInf :: foreach {k :: Nat}. Vec Char k -> Nat
localInf _ = k
bad :: Nat
bad = localInf @Zero VNil
HS
if "${ghc[@]}" -O0 -fno-code -c BadInferredAppLocal.hs >bad2.log 2>&1; then
    cat bad2.log >&2
    exit 1
fi

# 5b. @pat must be rejected on InferredSpec foreach {k :: Nat}.
cat >BadInferredPat.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module BadInferredPat where
import NativeProposal378QuantifiersProvider (Nat(..), Vec(..))
badPat :: foreach {k :: Nat}. Vec Char k -> Nat
badPat @n _ = n
HS
if "${ghc[@]}" -O0 -fno-code -c BadInferredPat.hs >bad3.log 2>&1; then
    cat bad3.log >&2
    exit 1
fi

# 5c. Unsound (%Unmatchable ->) to (%Matchable ->) instantiation MUST remain rejected!
cat >BadUnmatchableToMatchable.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module BadUnmatchableToMatchable where
import Data.Kind (Type)
import GHC.Matchability (M, U)

wantMatchable :: forall (f :: Bool %M -> Type). f True -> Int
wantMatchable _ = 0

bad :: forall (f :: Bool %U -> Type). f True -> Int
bad x = wantMatchable @f x
HS
if "${ghc[@]}" -O0 -fno-code -c BadUnmatchableToMatchable.hs >bad4.log 2>&1; then
    cat bad4.log >&2
    exit 1
fi
grep -Fq '[GHC-83865]' bad4.log
