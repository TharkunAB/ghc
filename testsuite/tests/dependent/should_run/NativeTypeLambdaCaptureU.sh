#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

ghc=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint -fdiagnostics-color=never)
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

# 1. Negative controls must be rejected cleanly
mkdir -p "$out/neg"
cp "$src/NativeTypeLambdaCaptureU.hs" "$out/neg/"
(
  cd "$out/neg"
  if "${ghc[@]}" -O0 -DWRONG_LAMBDA_TARGET -c NativeTypeLambdaCaptureU.hs >wrong.log 2>&1 ||
     ! grep -Fq '[GHC-25897]' wrong.log; then
    cat wrong.log >&2
    exit 1
  fi
  if "${ghc[@]}" -O0 -DBAD_INVERSE_U -c NativeTypeLambdaCaptureU.hs >inv.log 2>&1 ||
     ! grep -Fq '[GHC-25897]' inv.log; then
    cat inv.log >&2
    exit 1
  fi
  if "${ghc[@]}" -O0 -DBAD_RIGID_SKOLEMS_U -c NativeTypeLambdaCaptureU.hs >rigid.log 2>&1 ||
     ! grep -Eq '\[GHC-(83865|25897)\]' rigid.log; then
    cat rigid.log >&2
    exit 1
  fi
  if "${ghc[@]}" -O0 -DBAD_FIXED_U_TO_M -c NativeTypeLambdaCaptureU.hs >utom.log 2>&1 ||
     ! grep -Fq '[GHC-83865]' utom.log; then
    cat utom.log >&2
    exit 1
  fi
)

# 2. Compile provider across -O0/-O2 and native / -fno-code / -fwrite-if-simplified-core,
#    audit interfaces for unintended function tyvars, and run source-hidden ordinary client.
for opt in -O0 -O2; do
  for mode in native nocode simpcore; do
    dir="$out/$opt/$mode"
    mkdir -p "$dir/provider" "$dir/client"
    cp "$src/NativeTypeLambdaCaptureU.hs" "$dir/provider/"
    extra=()
    case "$mode" in
      nocode)   extra=(-fno-code -fwrite-interface) ;;
      simpcore) extra=(-fwrite-if-simplified-core) ;;
    esac
    (cd "$dir/provider" && "${ghc[@]}" "$opt" "${extra[@]}" -c NativeTypeLambdaCaptureU.hs)
    "${ghc[@]}" --show-iface "$dir/provider/NativeTypeLambdaCaptureU.hi" > "$dir/provider.iface"
    # Ensure no unintended bare `p :: a -> Type` or silently quantified function tyvar leaked
    python3 - "$dir/provider.iface" <<'PY'
import re, sys
raw_lines = open(sys.argv[1]).read().splitlines()
decls = []
for line in raw_lines:
    if line.startswith("    ") and decls:
        decls[-1] += " " + line.strip()
    else:
        decls.append(line)
FORALL_RE = re.compile(r"forall\b([^.]*)\.")
ALLOWED = {"p", "rel", "f", "g"}
for line in decls:
    s = line.strip()
    if s.startswith(("type ", "data ", "axiom ", "$")):
        continue
    for m in FORALL_RE.finditer(s):
        seg = m.group(1)
        for token in re.findall(r"\{?\([^()]*(?:\([^()]*\)[^()]*)*\)\}?", seg):
            inner = token.strip("{}() ")
            if "::" in inner:
                v, k = [x.strip() for x in inner.split("::", 1)]
                if "->" in k and v not in ALLOWED:
                    sys.exit(f"Unintended function tyvar {v} :: {k} in {s}")
PY

    cp "$dir/provider/NativeTypeLambdaCaptureU.hi" "$dir/client/"
    cat > "$dir/client/Client.hs" <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes, PolyKinds, RankNTypes, TypeApplications, TypeOperators #-}
module Main where

import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality ((:~:) (Refl))
import NativeTypeLambdaCaptureU

eqToLeibnizU :: (x :~: y) -> LeibnizU x y
eqToLeibnizU Refl px = px

symViaLeibnizU :: (x :~: y) -> (y :~: x)
symViaLeibnizU xy = leibnizUToEq (leibnizUSym (eqToLeibnizU xy))

main :: IO ()
main = do
  let r1 = leibnizUSym @Int @Int (eqToLeibnizU Refl) @(ConstU Int) (42 :: Int)
      r2 = leibnizUSymSpec @String @String (eqToLeibnizU Refl) @(ConstU String) "ok"
      r3 = leibnizUTrans @Bool @Bool @Bool (eqToLeibnizU Refl) (eqToLeibnizU Refl) @(ConstU Bool) True
      Refl = symViaLeibnizU (Refl :: Int :~: Int)
      Ticket t1 = forwardPartial (Proxy @Char) (Ticket (Just (Right 7)) :: StampedTicket Char)
      Ticket t2 = forwardCaptureU (Proxy @(ConstU String)) (Ticket (Just "tag") :: ConstTicket String)
      w = matchableWrap (Just 'a')
  print (r1, r2, r3, t1, t2, w)
HS
    if [[ "$mode" == "nocode" ]]; then
      (cd "$dir/client" && "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c Client.hs)
    else
      (
        cd "$dir/client"
        "${ghc[@]}" "$opt" -c Client.hs
        "${ghc[@]}" "$opt" Client.o ../provider/NativeTypeLambdaCaptureU.o -o run
        test "$(./run)" = '(42,"ok",True,Just (Right 7),Just "tag",Just '\''a'\'')'
      )
    fi
  done
done

# 3. Interactive GHCi verification
mkdir -p "$out/ghci"
"${ghc[@]}" -XTypeApplications --interactive -ignore-dot-ghci \
  -odir "$out/ghci" -hidir "$out/ghci" -i -i"$src" \
  "$src/NativeTypeLambdaCaptureU.hs" \
  -e '(leibnizUSym @Int @Int (\f -> f) @(ConstU String) "ghciA", leibnizUSymSpec @Int @Int (\f -> f) @(ConstU String) "ghciB")' > "$out/ghci.out"
grep -Fxq '("ghciA","ghciB")' "$out/ghci.out"
