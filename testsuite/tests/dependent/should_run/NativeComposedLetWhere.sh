#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"

g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
flags=(-fomit-interface-pragmas -fno-write-if-simplified-core)

cp "$area/NativeComposedLetWhere.hs" .
cat >R04_ComposedIsoFields.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module R04_ComposedIsoFields where

import Data.Type.Equality ((:~:))

data Iso a b where
    MkIso :: foreach (to :: a -> b) -> foreach (from :: b -> a)
          -> (foreach (x :: a) -> from (to x) :~: x)
          -> (foreach (y :: b) -> to (from y) :~: y)
          -> Iso a b

pack :: forall a b c. foreach (f :: a -> b) -> foreach (g :: b -> a)
     -> foreach (h :: b -> c) -> foreach (k :: c -> b)
     -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
     -> (foreach (y :: c) -> h (f (g (k y))) :~: y)
     -> Iso a c
pack f g h k left right = MkIso @a @c to from left right
  where
    to :: a -> c
    to x = h (f x)
    from :: c -> a
    from y = g (k y)
HS

# 1. Unmodified R04_ComposedIsoFields.hs at -O0 and -O2 -fno-enable-rewrite-rules,
#    plus .hi readback, GHCi, and default -O2 rule-policy check.
for opt in -O0 "-O2 -fno-enable-rewrite-rules"; do
    read -ra opt_flags <<<"$opt"
    out_dir="r04_${opt//[^a-zA-Z0-9]/_}"
    mkdir -p "$out_dir" "$out_dir/readback"
    "${g[@]}" "${opt_flags[@]}" "${flags[@]}" -c -outputdir "$out_dir" R04_ComposedIsoFields.hs
    test -s "$out_dir/R04_ComposedIsoFields.hi"
    "${g[@]}" "${opt_flags[@]}" "${flags[@]}" -fno-code -fwrite-interface -fforce-recomp \
        -outputdir "$out_dir/readback" R04_ComposedIsoFields.hs
    test -s "$out_dir/readback/R04_ComposedIsoFields.hi"
    "$1" "-B$2" --show-iface "$out_dir/R04_ComposedIsoFields.hi" | grep -Fq 'pack ::'
done

# Default -O2 (with active rewrite rules) must reject FinalCoreSourceTable (.)
# and therefore fall back to GHC-83865 on R04_ComposedIsoFields.hs.
if "${g[@]}" -O2 "${flags[@]}" -fforce-recomp -c R04_ComposedIsoFields.hs >r04-o2def.log 2>&1; then
    echo "R04_ComposedIsoFields unexpectedly compiled under default -O2 with active rules" >&2
    exit 1
fi
grep -Fq 'error: [GHC-83865]' r04-o2def.log

# GHCi type check on unmodified R04_ComposedIsoFields.hs.
ghci_out=$("${g[@]}" -O0 R04_ComposedIsoFields.hs -e ':t pack')
echo "$ghci_out" | grep -Fq 'Iso a c'

# 2. Positive runtime + Core/STG Lint across let, where, 0-arg, 1-arg, chained, case.
for mode in O0 O2; do
    mkdir -p "run_$mode"
    if [[ $mode == O0 ]]; then
        opt_flags=(-O0)
    else
        opt_flags=(-O2 -fno-enable-rewrite-rules)
    fi
    "${g[@]}" "${opt_flags[@]}" "${flags[@]}" --make -outputdir "run_$mode" \
        NativeComposedLetWhere.hs -o "run_$mode/prog"
    "./run_$mode/prog" >"run_$mode/stdout"
done
diff -u run_O0/stdout run_O2/stdout
cat run_O0/stdout

# 3. Independent non-proof composition alias must still compile under default -O2.
cat >IndependentCompose.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module IndependentCompose (independent) where

independent :: (b -> c) -> (a -> b) -> a -> c
independent h f y = to y
  where
    to x = h (f x)

{-# RULES "compose/any" forall f g x. (f . g) x = f x #-}
HS
"${g[@]}" -O2 -Wno-inline-rule-shadowing -c IndependentCompose.hs

# 4. Strict negative controls.
reject() {
    local file=$1 code=$2
    shift 2
    local status=0
    "${g[@]}" "$@" -fforce-recomp -c "$file" >reject.log 2>&1 || status=$?
    if [[ $status != 1 ]] || ! grep -Fq "[$code]" reject.log; then
        echo "Expected failure [$code] for $file, got status=$status:" >&2
        cat reject.log >&2
        exit 1
    fi
}

# 4a. Wrong inverse proof (to x = h x instead of h (f x)).
cat >WrongProof.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module WrongProof where
import Data.Type.Equality ((:~:))

data Iso a b where
  MkIso :: foreach (to :: a -> b) -> foreach (from :: b -> a)
        -> (foreach (x :: a) -> from (to x) :~: x)
        -> (foreach (y :: b) -> to (from y) :~: y)
        -> Iso a b

bad :: forall a. foreach (f :: a -> a) -> foreach (g :: a -> a)
    -> foreach (h :: a -> a) -> foreach (k :: a -> a)
    -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
    -> (foreach (y :: a) -> h (f (g (k y))) :~: y)
    -> Iso a a
bad f g h k left right = MkIso @a @a to from left right
  where
    to :: a -> a
    to x = h x
    from :: a -> a
    from y = g (k y)
HS
reject WrongProof.hs GHC-05617 -O0

cat >WrongProofSingle.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module WrongProofSingle where
import Data.Type.Equality ((:~:))

data Iso a b where
  MkIso :: foreach (to :: a -> b) -> foreach (from :: b -> a)
        -> (foreach (x :: a) -> from (to x) :~: x)
        -> (foreach (y :: b) -> to (from y) :~: y)
        -> Iso a b

badSingle :: forall a. foreach (f :: a -> a) -> foreach (g :: a -> a)
          -> foreach (h :: a -> a)
          -> (foreach (x :: a) -> g (h (f x)) :~: x)
          -> (foreach (y :: a) -> h (f (g y)) :~: y)
          -> Iso a a
badSingle f g h left right = MkIso @a @a to g left right
  where
    to :: a -> a
    to x = h x
HS
reject WrongProofSingle.hs GHC-83865 -O0

# 4b. Recursive local function must NOT get a composition alias.
cat >RecursiveLocal.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module RecursiveLocal where
import Data.Type.Equality ((:~:))

data Iso a b where
  MkIso :: foreach (to :: a -> b) -> foreach (from :: b -> a)
        -> (foreach (x :: a) -> from (to x) :~: x)
        -> (foreach (y :: b) -> to (from y) :~: y)
        -> Iso a b

badRec :: forall a. foreach (h :: a -> a) -> foreach (k :: a -> a)
       -> (foreach (x :: a) -> k (h (h x)) :~: x)
       -> (foreach (y :: a) -> h (h (k y)) :~: y)
       -> Iso a a
badRec h k left right = MkIso @a @a to k left right
  where
    to :: a -> a
    to x = h (to x)
HS
reject RecursiveLocal.hs GHC-83865 -O0

# 4c. Banged pattern `to !x = h (f x)` must NOT alias to lazy (.) h f.
cat >BangPatternReject.hs <<'HS'
{-# LANGUAGE BangPatterns, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module BangPatternReject where
import Data.Type.Equality ((:~:))

data Iso a b where
  MkIso :: foreach (to :: a -> b) -> foreach (from :: b -> a)
        -> (foreach (x :: a) -> from (to x) :~: x)
        -> (foreach (y :: b) -> to (from y) :~: y)
        -> Iso a b

badBang :: forall a. foreach (f :: a -> a) -> foreach (g :: a -> a)
        -> foreach (h :: a -> a)
        -> (foreach (x :: a) -> g (h (f x)) :~: x)
        -> (foreach (y :: a) -> h (f (g y)) :~: y)
        -> Iso a a
badBang f g h left right = MkIso @a @a to g left right
  where
    to :: a -> a
    to !x = h (f x)
HS
reject BangPatternReject.hs GHC-83865 -O0

# 4d. Strict extension `to x = h (f x)` must NOT alias to lazy (.) h f.
cat >IsoBase.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, TypeOperators #-}
module IsoBase (Iso(..)) where
import Data.Type.Equality ((:~:))

data Iso a b where
  MkIso :: foreach (to :: a -> b) -> foreach (from :: b -> a)
        -> (foreach (x :: a) -> from (to x) :~: x)
        -> (foreach (y :: b) -> to (from y) :~: y)
        -> Iso a b
HS
"${g[@]}" -O0 -fforce-recomp -c IsoBase.hs

cat >StrictExtReject.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, ScopedTypeVariables, Strict, TypeApplications, TypeOperators #-}
module StrictExtReject where
import Data.Type.Equality ((:~:))
import IsoBase (Iso(..))

badStrict :: forall a. foreach (f :: a -> a) -> foreach (g :: a -> a)
          -> foreach (h :: a -> a)
          -> (foreach (x :: a) -> g (h (f x)) :~: x)
          -> (foreach (y :: a) -> h (f (g y)) :~: y)
          -> Iso a a
badStrict f g h left right = MkIso @a @a to g left right
  where
    to :: a -> a
    to x = h (f x)
HS
reject StrictExtReject.hs GHC-83865 -O0

# 4e. Runtime strictness check: banged/Strict `to` MUST force bottom argument.
cat >StrictLocalMod.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, Strict #-}
module StrictLocalMod (strictLocal) where

strictLocal :: Int
strictLocal = to (error "strict-forced")
  where
    to x = const 1 (id x)
HS
cat >StrictRuntime.hs <<'HS'
{-# LANGUAGE BangPatterns, ExperimentalDependentTypes #-}
module Main where
import Control.Exception (ErrorCall, catch, evaluate)
import StrictLocalMod (strictLocal)

bangLocal :: Int
bangLocal = to (error "bang-forced")
  where
    to !x = const 1 (id x)

expectBottom :: IO Int -> IO String
expectBottom act = (show <$> (act >>= evaluate)) `catch` \(_ :: ErrorCall) -> pure "bottom"

main :: IO ()
main = do
  r1 <- expectBottom (pure bangLocal)
  r2 <- expectBottom (pure strictLocal)
  putStrLn (r1 ++ ":" ++ r2)
HS
"${g[@]}" -O0 --make StrictRuntime.hs -o strict-run
[[ $(./strict-run) == "bottom:bottom" ]]

# 4f. Conflicting RULES pragma on (.) -> GHC-83605 on direct (.), and rejected in where.
cat >ComposeRuleDirect.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module ComposeRuleDirect where
import Data.Type.Equality ((:~:))

data Iso a b where
  MkIso :: foreach (to :: a -> b) -> foreach (from :: b -> a)
        -> (foreach (x :: a) -> from (to x) :~: x)
        -> (foreach (y :: b) -> to (from y) :~: y)
        -> Iso a b

badRule :: forall a b c. foreach (f :: a -> b) -> foreach (g :: b -> a)
        -> foreach (h :: b -> c) -> foreach (k :: c -> b)
        -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
        -> (foreach (y :: c) -> h (f (g (k y))) :~: y)
        -> Iso a c
badRule f g h k left right = MkIso @a @c (h . f) (g . k) left right

{-# RULES "bad compose" forall f g x. (f . g) x = f x #-}
HS
reject ComposeRuleDirect.hs GHC-83605 -O0 -fenable-rewrite-rules -Wno-inline-rule-shadowing

cat >ComposeRuleWhere.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module ComposeRuleWhere where
import Data.Type.Equality ((:~:))

data Iso a b where
  MkIso :: foreach (to :: a -> b) -> foreach (from :: b -> a)
        -> (foreach (x :: a) -> from (to x) :~: x)
        -> (foreach (y :: b) -> to (from y) :~: y)
        -> Iso a b

badRuleWhere :: forall a. foreach (f :: a -> a) -> foreach (g :: a -> a)
             -> foreach (h :: a -> a)
             -> (foreach (x :: a) -> g (h (f x)) :~: x)
             -> (foreach (y :: a) -> h (f (g y)) :~: y)
             -> Iso a a
badRuleWhere f g h left right = MkIso @a @a to g left right
  where
    to x = h (f x)

{-# RULES "bad compose where" forall f g x. (f . g) x = f x #-}
HS
reject ComposeRuleWhere.hs GHC-83605 -O0 -fenable-rewrite-rules -Wno-inline-rule-shadowing
