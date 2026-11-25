#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

ghc_bin=$1
ghc_lib=$2
shift 2

base_ghc=("$ghc_bin" "-B$ghc_lib" -v0 -package-env - -i -i.
          -dcore-lint -dstg-lint -fdiagnostics-color=never
          -Wall -Wno-name-shadowing -Werror "$@")

provider=NativeMultiParamMethodOpenEqProvider
client=NativeMultiParamMethodOpenEq

# 1. Stripped-interface (.hi-only) compilation + execution at -O0 and -O2.
for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cp "$src/$provider.hs" "$out/$opt/provider/"
    (
        cd "$out/$opt/provider"
        "${base_ghc[@]}" "-$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$provider.hs"
    )
    cp "$out/$opt/provider/$provider.hi" "$src/$client.hs" "$out/$opt/client/"
    (
        cd "$out/$opt/client"
        "${base_ghc[@]}" "-$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$client.hs"
        "${base_ghc[@]}" "-$opt" "$client.o" "../provider/$provider.o" -o run
        ./run
    )
done

# 2. -fno-code -fwrite-interface on client against stripped provider .hi.
mkdir -p "$out/nocode"
cp "$out/O0/provider/$provider.hi" "$src/$client.hs" "$out/nocode/"
(
    cd "$out/nocode"
    "${base_ghc[@]}" -O0 -fno-code -fwrite-interface -c "$client.hs"
)

# 3. GHCi (--interactive) load and execution.
mkdir -p "$out/ghci"
cp "$src/$provider.hs" "$src/$client.hs" "$out/ghci/"
(
    cd "$out/ghci"
    printf ':load %s.hs\nmain\n:quit\n' "$client" | \
        "$ghc_bin" "-B$ghc_lib" -v0 -ignore-dot-ghci -package-env - --interactive >ghci.out 2>&1
    if [[ -s ghci.out ]]; then
        cat ghci.out >&2
        exit 1
    fi
)

# 4. Negative controls (against stripped provider .hi):
mkdir -p "$out/neg"
cp "$out/O0/provider/$provider.hi" "$out/neg/"

# 4a. Local Given dictionary masks global multi-parameter instance.
cat >"$out/neg/GivenMask.hs" <<'HS'
{-# LANGUAGE DependentTypes, FlexibleContexts, GADTs, ListTuplePuns, ScopedTypeVariables #-}
module GivenMask where
import Data.Type.Equality ((:~:) (Refl))
import NativeMultiParamMethodOpenEqProvider

data Dict c where
  Dict :: c => Dict c

masked :: forall e. Dict (LabelledMap e (Either e)) -> ()
masked Dict =
  let inner :: foreach (f :: a -> b) -> foreach (x :: a)
            -> lmap f (Right x :: Either e a) :~: Right (f x)
      inner _ _ = Refl
  in inner id True `seq` ()
HS
if (cd "$out/neg" && "${base_ghc[@]}" -O0 -Wwarn -Wno-simplifiable-class-constraints -c GivenMask.hs >given.log 2>&1) ||
   ! grep -Fq 'This call needs a known' "$out/neg/given.log"; then
    cat "$out/neg/given.log" >&2
    exit 1
fi

# 4b. Competing non-apart multi-parameter instance at polymorphic receiver is rejected.
cat >"$out/neg/NonApart.hs" <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NonApart where
import Data.Type.Equality ((:~:) (Refl))
import NativeMultiParamMethodOpenEqProvider

ambiguous :: foreach (f :: a -> b) -> foreach (x :: a)
          -> omap f (Right x :: Either e a) :~: Right (f x)
ambiguous _ _ = Refl
HS
if (cd "$out/neg" && "${base_ghc[@]}" -O0 -Wwarn -c NonApart.hs >apart.log 2>&1); then
    echo "ERROR: NonApart unexpectedly succeeded" >&2
    exit 1
fi

# 4c. Active rewrite rule on multi-parameter method selector rejects with GHC-83605.
cat >"$out/neg/MethodRule.hs" <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module MethodRule where
import Data.Type.Equality ((:~:) (Refl))
import NativeMultiParamMethodOpenEqProvider

{-# RULES "tappend/zero" forall x. tappend x MZero = MZero #-}

badRule :: foreach (n :: MyNat) -> tappend MZero n :~: n
badRule _ = Refl
HS
if (cd "$out/neg" && "${base_ghc[@]}" -O1 -Wwarn -c MethodRule.hs >mrule.log 2>&1) ||
   ! grep -Fq '[GHC-83605]' "$out/neg/mrule.log"; then
    cat "$out/neg/mrule.log" >&2
    exit 1
fi

# 4d. Wrong equation on certified Eq (==) rejects with GHC-83865.
cat >"$out/neg/WrongEq.hs" <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module WrongEq where
import Data.Type.Equality ((:~:) (Refl))
import NativeMultiParamMethodOpenEqProvider

badEq :: foreach (n :: MyNat) -> (n == n) :~: False
badEq MZero = Refl
badEq (MSucc _) = Refl
HS
if (cd "$out/neg" && "${base_ghc[@]}" -O0 -Wwarn -c WrongEq.hs >wrongeq.log 2>&1) ||
   ! grep -Eq '\[GHC-(83865|25897|05617)\]' "$out/neg/wrongeq.log"; then
    cat "$out/neg/wrongeq.log" >&2
    exit 1
fi
