#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

ghc_bin=$1
libdir=$2
ghc_pkg=${3:-"$(dirname "$ghc_bin")/ghc-pkg"}
if [[ ! -x "$ghc_pkg" ]]; then
  ghc_pkg=$(realpath "$libdir/../bin/ghc-pkg")
fi

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
obs_eq_dir=${DEPENDENT_OBS_EQ_DIR:-/home/tritlo/Code/dependent-obs-eq}
if [[ ! -f "$obs_eq_dir/dependent-obs-eq.cabal" ]]; then
  echo "Missing dependent-obs-eq repository at $obs_eq_dir" >&2
  exit 1
fi

out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

comp_dir=$(cd -- "$(dirname -- "$ghc_bin")" && pwd)
pkg_db="$out/pkg.conf.d"
mkdir -p "$pkg_db"
for conf in "$libdir/package.conf.d"/*.conf; do
  bname=$(basename "$conf")
  if [[ -d "$comp_dir/modules" && -d "$comp_dir/package" && "$bname" == ghc-10.*.conf ]]; then
    python3 - "$conf" "$pkg_db/$bname" "$libdir" "$comp_dir" <<'PY'
from pathlib import Path
from sys import argv
src_p, dst_p, libdir, comp = argv[1:]
txt = Path(src_p).read_text().replace("${pkgroot}", libdir)
txt = txt.replace("import-dirs:", f"import-dirs:\n    {comp}/modules", 1)
txt = txt.replace("library-dirs:", f"library-dirs:\n    {comp}/package", 1)
txt = txt.replace("dynamic-library-dirs:", f"dynamic-library-dirs: {comp}/package ", 1)
Path(dst_p).write_text(txt)
PY
  else
    sed "s|\${pkgroot}|$libdir|g" "$conf" > "$pkg_db/$bname"
  fi
done
"$ghc_pkg" --package-db "$pkg_db" recache >/dev/null
pkg_flags=(-clear-package-db -package-db "$pkg_db")
base_ghc=("$ghc_bin" "-B$libdir" -v0 -package-env - "${pkg_flags[@]}" -fdiagnostics-color=never)

# 1. Build and register dependent-obs-eq into the temporary package DB.
plug_lib="$out/obs-eq-lib"
mkdir -p "$plug_lib"
unit_id="dependent-obs-eq-0.1.0.0"
ghc_ver=$("$ghc_bin" "-B$libdir" --numeric-version)
so_name="libHS${unit_id}-ghc${ghc_ver}.so"

"${base_ghc[@]}" -O2 -Wall -Werror -haddock -XGHC2024 -this-unit-id "$unit_id" \
  -package base -package containers -package ghc -package time \
  -i"$obs_eq_dir/src" -odir "$plug_lib" -hidir "$plug_lib" \
  -c "$obs_eq_dir/src/Dependent/ObsEq.hs" \
     "$obs_eq_dir/src/Dependent/Plugin/ObsEq.hs"

"${base_ghc[@]}" -O2 -Wall -Werror -haddock -XGHC2024 -dynamic -fPIC -osuf dyn_o -hisuf dyn_hi \
  -this-unit-id "$unit_id" -package base -package containers -package ghc -package time \
  -i"$obs_eq_dir/src" -odir "$plug_lib" -hidir "$plug_lib" \
  -c "$obs_eq_dir/src/Dependent/ObsEq.hs" \
     "$obs_eq_dir/src/Dependent/Plugin/ObsEq.hs"

ar rcs "$plug_lib/libHS${unit_id}.a" \
  "$plug_lib/Dependent/ObsEq.o" \
  "$plug_lib/Dependent/Plugin/ObsEq.o"

"${base_ghc[@]}" -shared -dynamic -no-hs-main \
  -package base -package containers -package ghc -package time \
  "-optl-Wl,-soname,$so_name" \
  "$plug_lib/Dependent/ObsEq.dyn_o" \
  "$plug_lib/Dependent/Plugin/ObsEq.dyn_o" \
  -o "$plug_lib/$so_name"

base_id=$("$ghc_pkg" --package-db "$pkg_db" field base id --simple-output | head -n1)
cont_id=$("$ghc_pkg" --package-db "$pkg_db" field containers id --simple-output | head -n1)
ghc_id=$("$ghc_pkg" --package-db "$pkg_db" field ghc id --simple-output | head -n1)
time_id=$("$ghc_pkg" --package-db "$pkg_db" field time id --simple-output | head -n1)

cat <<EOF | "$ghc_pkg" --package-db "$pkg_db" register - >/dev/null
name: dependent-obs-eq
version: 0.1.0.0
id: $unit_id
key: $unit_id
exposed: True
exposed-modules: Dependent.ObsEq Dependent.Plugin.ObsEq
import-dirs: $plug_lib
library-dirs: $plug_lib
dynamic-library-dirs: $plug_lib
hs-libraries: HS${unit_id}
depends: $base_id $cont_id $ghc_id $time_id
EOF

export LD_LIBRARY_PATH="$plug_lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

# 2. Provider module compiled WITHOUT any plugin (mixes certified plus with uncertified functions).
prov_src="$out/prov-src"
mkdir -p "$prov_src"
cat > "$prov_src/ObsProvider.hs" <<'HS'
{-# LANGUAGE DependentTypes #-}
module ObsProvider (Nat(..), plus, natToInt, stepUnfoldAdd, whileAdd) where

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

stepUnfoldAdd :: Nat -> Nat -> Nat
stepUnfoldAdd m n = step (Just m) n
  where
    step Nothing acc = acc
    step (Just Zero) acc = step Nothing acc
    step (Just (Succ k)) acc = Succ (step (Just k) acc)

whileAdd :: Nat -> Nat -> Nat
whileAdd Zero acc = acc
whileAdd (Succ m) acc = whileAdd m (Succ acc)
HS

"${base_ghc[@]}" -O2 -package base -XGHC2024 \
  -c "$prov_src/ObsProvider.hs" \
  -o "$prov_src/ObsProvider.o" -ohi "$prov_src/ObsProvider.hi"

"${base_ghc[@]}" -O2 -dynamic -fPIC -osuf dyn_o -hisuf dyn_hi \
  -package base -XGHC2024 \
  -c "$prov_src/ObsProvider.hs" \
  -o "$prov_src/ObsProvider.dyn_o" -ohi "$prov_src/ObsProvider.dyn_hi"

# 3. Positive end-to-end compilation & execution at -O0 and -O2 with -dcore-lint -dstg-lint,
#    plus standalone dependent-obs-eq test/Main.hs!
for opt in -O0 -O2; do
  run_dir="$out/pos/$opt"
  mkdir -p "$run_dir"
  "${base_ghc[@]}" "$opt" -dynamic --make -dcore-lint -dstg-lint \
    -package base -package ghc-experimental -package dependent-obs-eq \
    -XGHC2024 -i -i"$prov_src" \
    -odir "$run_dir/build" -hidir "$run_dir/build" \
    "$src/NativeObsEqPlugin.hs" -o "$run_dir/obs-eq-prog"
  "$run_dir/obs-eq-prog"

  pkg_test_dir="$out/pkg-test/$opt"
  mkdir -p "$pkg_test_dir"
  "${base_ghc[@]}" "$opt" -dynamic --make -dcore-lint -dstg-lint \
    -package base -package ghc-experimental -package dependent-obs-eq \
    -XGHC2024 -odir "$pkg_test_dir/build" -hidir "$pkg_test_dir/build" \
    "$obs_eq_dir/test/Main.hs" -o "$pkg_test_dir/pkg-test"
  "$pkg_test_dir/pkg-test" >/dev/null
done

expect_fail_grep() {
  local label=$1
  local pattern=$2
  shift 2
  local log="$out/${label}.log"
  if "$@" >"$log" 2>&1; then
    echo "Expected compilation failure for $label, but it succeeded!" >&2
    exit 1
  fi
  if ! tr '\n' ' ' < "$log" | grep -Eq "$pattern"; then
    echo "Missing expected pattern '$pattern' in $label:" >&2
    cat "$log" >&2
    exit 1
  fi
}

neg_src="$out/neg"
mkdir -p "$neg_src"

stage_dir() {
  local d="$out/$1"
  mkdir -p "$d"
  cp "$prov_src"/ObsProvider.* "$d/"
  echo "$d"
}

# 4. No-plugin negative controls: uncertified local -> GHC-68947, imported -> GHC-70565.
cat > "$neg_src/NoPluginLocal.hs" <<'HS'
{-# LANGUAGE DependentTypes #-}
module NoPluginLocal where
import Data.Type.Equality ((:~:))
import ObsProvider (Nat(..))

badLoop :: Nat -> Nat
badLoop n = badLoop n

law :: foreach (n :: Nat) -> badLoop n :~: Zero
law = error "unreachable"
HS

d_np1=$(stage_dir np1)
expect_fail_grep "no-plugin-local" '\[GHC-68947\]' \
  "${base_ghc[@]}" -O0 -c -fno-code -package base -package ghc-experimental \
  -XGHC2024 -i -i"$prov_src" -odir "$d_np1" -hidir "$d_np1" \
  "$neg_src/NoPluginLocal.hs"

cat > "$neg_src/NoPluginImported.hs" <<'HS'
{-# LANGUAGE DependentTypes #-}
module NoPluginImported where
import Data.Type.Equality ((:~:))
import ObsProvider (Nat(..), plus, whileAdd)

law :: foreach (m :: Nat) (n :: Nat) -> whileAdd m n :~: plus m n
law = error "unreachable"
HS

d_np2=$(stage_dir np2)
expect_fail_grep "no-plugin-imported" '\[GHC-70565\]' \
  "${base_ghc[@]}" -O0 -dynamic -osuf dyn_o -hisuf dyn_hi -c -fno-code \
  -package base -package ghc-experimental \
  -XGHC2024 -i -i"$prov_src" -odir "$d_np2" -hidir "$d_np2" \
  "$neg_src/NoPluginImported.hs"

# 5. Domain-restricted evenOnly refuted on arbitrary Ints (both Refl and obsEq!).
cat > "$neg_src/EvenRefuted.hs" <<'HS'
{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -fplugin=Dependent.Plugin.ObsEq #-}
module EvenRefuted where
import Data.Type.Equality ((:~:))
import Dependent.ObsEq (obsEq)

evenOnly :: Int -> Int
evenOnly n = if even n then (n `quot` 2) * 2 else n + 1

badEven :: foreach (n :: Int) -> evenOnly n :~: n
badEven n = obsEq
HS

mkdir -p "$out/er"
expect_fail_grep "even-refuted" 'Observational equivalence refuted' \
  "${base_ghc[@]}" -O0 -dynamic -osuf dyn_o -hisuf dyn_hi -c -fno-code \
  -package base -package ghc-experimental -package dependent-obs-eq \
  -XGHC2024 -odir "$out/er" -hidir "$out/er" \
  "$neg_src/EvenRefuted.hs"

# 6. Partiality & laziness mismatch (ChasingBottoms / ObsBottom):
#    strictFirst x y = seq y x vs const x y agrees on all total (x, y)
#    but is falsified on y = <bottom>!
cat > "$neg_src/StrictVsLazy.hs" <<'HS'
{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -fplugin=Dependent.Plugin.ObsEq #-}
module StrictVsLazy where
import Data.Type.Equality ((:~:))
import Dependent.ObsEq (obsEq)
import ObsProvider (Nat(..))

strictFirst :: a -> b -> a
strictFirst x y = seq y x

badStrict :: foreach (x :: Nat) (y :: Nat) -> strictFirst x y :~: const x y
badStrict x y = obsEq
HS

d_svl=$(stage_dir svl)
expect_fail_grep "strict-vs-lazy" 'Observational equivalence refuted.*<bottom#' \
  "${base_ghc[@]}" -O0 -dynamic -osuf dyn_o -hisuf dyn_hi -c -fno-code \
  -package base -package ghc-experimental -package dependent-obs-eq \
  -XGHC2024 -i -i"$prov_src" -odir "$d_svl" -hidir "$d_svl" \
  "$neg_src/StrictVsLazy.hs"

# 7. Non-termination safety (timeout): loop n ~ Zero fails quickly with <timeout>.
cat > "$neg_src/LoopVsZero.hs" <<'HS'
{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -fplugin=Dependent.Plugin.ObsEq -fplugin-opt=Dependent.Plugin.ObsEq:timeout-us=2000 #-}
module LoopVsZero where
import Data.Type.Equality ((:~:))
import Dependent.ObsEq (obsEq)
import ObsProvider (Nat(..))

loop :: Nat -> Nat
loop n = loop n

badLoop :: foreach (n :: Nat) -> loop n :~: Zero
badLoop n = obsEq
HS

d_lvz=$(stage_dir lvz)
start_ts=$(date +%s)
expect_fail_grep "loop-vs-zero" 'Observational equivalence refuted.*<timeout>' \
  "${base_ghc[@]}" -O0 -dynamic -osuf dyn_o -hisuf dyn_hi -c -fno-code \
  -package base -package ghc-experimental -package dependent-obs-eq \
  -XGHC2024 -i -i"$prov_src" -odir "$d_lvz" -hidir "$d_lvz" \
  "$neg_src/LoopVsZero.hs"
end_ts=$(date +%s)
if (( end_ts - start_ts > 15 )); then
  echo "LoopVsZero took too long ($((end_ts - start_ts))s)!" >&2
  exit 1
fi

# 8. Option 1 vs Option 2 seam (ExplicitOnly default vs policy=assume):
cat > "$neg_src/BareReflAssume.hs" <<'HS'
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fplugin=Dependent.Plugin.ObsEq #-}
module Main (main) where
import Data.Type.Equality ((:~:) (Refl))
import ObsProvider (Nat(..), plus, stepUnfoldAdd)

bareLaw :: foreach (m :: Nat) (n :: Nat) -> stepUnfoldAdd m n :~: plus m n
bareLaw m n = Refl

main :: IO ()
main = case bareLaw (Succ Zero) (Succ Zero) of Refl -> pure ()
HS

# 8a. Under default ExplicitOnly policy, bare Refl MUST be rejected with a nominal mismatch!
d_brexp=$(stage_dir br-exp)
expect_fail_grep "bare-refl-explicit" 'Couldn.t match type' \
  "${base_ghc[@]}" -O0 -dynamic -osuf dyn_o -hisuf dyn_hi -c -fno-code \
  -package base -package ghc-experimental -package dependent-obs-eq \
  -XGHC2024 -i -i"$prov_src" -odir "$d_brexp" -hidir "$d_brexp" \
  "$neg_src/BareReflAssume.hs"

# 8b. Under explicit policy=assume, bare Refl discharges cleanly at -O0 and -O2 with -dcore-lint!
for opt in -O0 -O2; do
  mkdir -p "$out/br-ass/$opt"
  "${base_ghc[@]}" "$opt" -dynamic --make -dcore-lint -dstg-lint \
    -package base -package ghc-experimental -package dependent-obs-eq \
    -fplugin-opt=Dependent.Plugin.ObsEq:policy=assume \
    -XGHC2024 -i -i"$prov_src" \
    -odir "$out/br-ass/$opt/build" -hidir "$out/br-ass/$opt/build" \
    "$neg_src/BareReflAssume.hs" -o "$out/br-ass/$opt/prog"
  "$out/br-ass/$opt/prog"
done

# 8c. Even under policy=assume, false bare Refl (strictFirst vs const) MUST be refuted!
cat > "$neg_src/BareReflFalseAssume.hs" <<'HS'
{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -fplugin=Dependent.Plugin.ObsEq -fplugin-opt=Dependent.Plugin.ObsEq:policy=assume #-}
module BareReflFalseAssume where
import Data.Type.Equality ((:~:) (Refl))
import ObsProvider (Nat(..))

strictFirst :: a -> b -> a
strictFirst x y = seq y x

badBare :: foreach (x :: Nat) (y :: Nat) -> strictFirst x y :~: const x y
badBare x y = Refl
HS

d_brfa=$(stage_dir br-fa)
expect_fail_grep "bare-false-assume" 'Observational equivalence refuted.*<bottom#' \
  "${base_ghc[@]}" -O0 -dynamic -osuf dyn_o -hisuf dyn_hi -c -fno-code \
  -package base -package ghc-experimental -package dependent-obs-eq \
  -XGHC2024 -i -i"$prov_src" -odir "$d_brfa" -hidir "$d_brfa" \
  "$neg_src/BareReflFalseAssume.hs"
