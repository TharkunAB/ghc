#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
trap 'echo "NativeProposal378PatternsAndT2T.sh failed at line $LINENO: $BASH_COMMAND" >&2' ERR

inputs=$PWD
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

expected='(([0,0],[0,0],1,[17]),"qqqqq",(42,True,"other",[0],[8],[9],(10,True,"two+"),([7],[100,200]),(0,11,33),(Zero,Succ Zero)),(Succ (Succ Zero),Succ (Succ (Succ Zero)),"[True,False]",(Succ (Succ Zero),[True,False]),[77],(Succ Zero,[66]),[88]),15,([3,4],[True,False]),("42","True",1,2))'

for opt in -O0 -O2; do
    dir="$work/$opt"
    mkdir -p "$dir"/{native,readback,neg}

    # 1. Compile Provider + Client and run binary
    cd "$dir/native"
    cp "$inputs/NativeProposal378PatternsAndT2TProvider.hs" \
       "$inputs/NativeProposal378PatternsAndT2T.hs" .
    "${ghc[@]}" "$opt" -c NativeProposal378PatternsAndT2TProvider.hs
    "${ghc[@]}" "$opt" -c NativeProposal378PatternsAndT2T.hs
    "${ghc[@]}" "$opt" NativeProposal378PatternsAndT2TProvider.o \
        NativeProposal378PatternsAndT2T.o -o run
    actual=$(./run)
    if [[ "$actual" != "$expected" ]]; then
        echo "Unexpected output at $opt:" >&2
        echo "  expected: $expected" >&2
        echo "    actual: $actual" >&2
        exit 1
    fi

    # 2. Separate .hi readback check without Provider.hs source
    cd "$dir/readback"
    cp "$dir/native/NativeProposal378PatternsAndT2TProvider.hi" \
       "$inputs/NativeProposal378PatternsAndT2T.hs" .
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c NativeProposal378PatternsAndT2T.hs
done

# 3. Negative checks
cd "$work/-O0/neg"
cp "$work/-O0/native/NativeProposal378PatternsAndT2TProvider.hi" .

# 3a. Unadorned foreach in actual value expression still rejected with GHC-31244
cat >BadValueForEach.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module BadValueForEach where
import NativeProposal378PatternsAndT2TProvider
bad = (foreach (n :: Nat) -> Vec Int n)
HS
if "${ghc[@]}" -c BadValueForEach.hs >bad1.out 2>bad1.err; then
    echo "Expected BadValueForEach.hs to fail" >&2
    exit 1
fi
grep -Fq "[GHC-31244]" bad1.err

# 3b. Self-referencing local signature still rejected with GHC-83605
cat >BadSelfLocalSig.hs <<'HS'
{-# LANGUAGE DependentTypes #-}
module BadSelfLocalSig where
import NativeProposal378PatternsAndT2TProvider
bad :: Int
bad =
  let n :: Vec Int n
      n = error "impossible"
  in 0
HS
if "${ghc[@]}" -c BadSelfLocalSig.hs >bad2.out 2>bad2.err; then
    echo "Expected BadSelfLocalSig.hs to fail" >&2
    exit 1
fi
grep -Fq "[GHC-83605]" bad2.err

# 3c. @arg on InferredSpec GADT constructor field foreach {n :: Nat}. rejected
cat >BadInferredConField.hs <<'HS'
{-# LANGUAGE DependentTypes, TypeApplications #-}
module BadInferredConField where
import NativeProposal378PatternsAndT2TProvider
bad :: InvisVec Bool
bad = MkInvisInf @Bool @Zero VNil
HS
if "${ghc[@]}" -c BadInferredConField.hs >bad3.out 2>bad3.err; then
    echo "Expected BadInferredConField.hs to fail" >&2
    exit 1
fi
