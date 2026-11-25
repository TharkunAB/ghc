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
repo_root=$(cd -- "$src/../../../.." && pwd)
dep_base_src="${DEPENDENT_BASE_DIR:-/home/tritlo/Code/dependent-base}/src"
if [[ -d "$src/open-evaluation" ]]; then
  open_eval_dir="$src/open-evaluation"
else
  open_eval_dir="$repo_root/docs/dependent-haskell/examples/open-evaluation"
fi

out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

comp_dir=$(cd -- "$(dirname -- "$ghc_bin")" && pwd)
pkg_db="$out/pkg.conf.d"
mkdir -p "$pkg_db"
for conf in "$libdir/package.conf.d"/*.conf; do
  bname=$(basename "$conf")
  if [[ -d "$comp_dir/modules" && -d "$comp_dir/package" && "$bname" == ghc-[0-9]*.conf ]]; then
    sed "s|\${pkgroot}|$libdir|g; s|^import-dirs:|import-dirs: $comp_dir/modules |; s|^library-dirs:|library-dirs: $comp_dir/package |; s|^dynamic-library-dirs:|dynamic-library-dirs: $comp_dir/package |" \
      "$conf" > "$pkg_db/$bname"
  else
    sed "s|\${pkgroot}|$libdir|g" "$conf" > "$pkg_db/$bname"
  fi
done
"$ghc_pkg" --package-db "$pkg_db" recache >/dev/null
pkg_flags=(-clear-package-db -package-db "$pkg_db")

base_ghc=("$ghc_bin" "-B$libdir" -v0 -package-env - "${pkg_flags[@]}" -fdiagnostics-color=never)

# 1. Direct GHC API check of hook predicates at both -O0 and -O2.
for opt in -O0 -O2; do
  mkdir -p "$out/api/$opt"
  "${base_ghc[@]}" "$opt" -dynamic -package ghc \
    "$src/NativePluginsGeneral.hs" -outputdir "$out/api/$opt" -o "$out/api/$opt/api-check"
  "$out/api/$opt/api-check"
done

# 2. Build and register a dynamic plugin package exercising all plugin categories:
#    (a) TestTcPlugin          (tcPlugin)
#    (b) TestSourcePlugin      (parsedResultAction + typeCheckResultAction + interfaceLoadAction)
#    (c) TestHoleFitPlugin     (holeFitPlugin)
#    (d) TestCoreInspectPlugin (installCoreToDos read-only Core inspection pass)
#    (e) TestCoreMutatePlugin  (installCoreToDos negative control mutating Core RHSs!)
#    (f) TestLatePlugin        (latePlugin negative control)
plug_src="$out/plug-src"
plug_lib="$out/plug-lib"
mkdir -p "$plug_src" "$plug_lib"

cat > "$plug_src/TestTcPlugin.hs" <<'HS'
{-# LANGUAGE Haskell2010 #-}
module TestTcPlugin (plugin) where

import GHC.Plugins
import GHC.Tc.Types (TcPlugin(..), TcPluginSolveResult(..), unsafeTcPluginTcM)

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = \opts -> Just $ TcPlugin
      { tcPluginInit    = unsafeTcPluginTcM $ liftIO $ do
          case opts of
            path : _ -> appendFile path "tc-init\n"
            []       -> pure ()
      , tcPluginSolve    = \_ _ _ _ -> pure (TcPluginOk [] [])
      , tcPluginRewrite  = \_ -> emptyUFM
      , tcPluginPostTc   = \_ -> pure ()
      , tcPluginShutdown = \_ -> pure ()
      }
  , pluginRecompile = purePlugin
  }
HS

cat > "$plug_src/TestSourcePlugin.hs" <<'HS'
{-# LANGUAGE Haskell2010 #-}
module TestSourcePlugin (plugin) where

import GHC.Plugins

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \opts ms parsed -> do
      case opts of
        path : _ -> liftIO $ appendFile path ("parsed:" ++ moduleNameString (ms_mod_name ms) ++ "\n")
        []       -> pure ()
      pure parsed
  , typeCheckResultAction = \opts ms env -> do
      case opts of
        path : _ -> liftIO $ appendFile path ("typechecked:" ++ moduleNameString (ms_mod_name ms) ++ "\n")
        []       -> pure ()
      pure env
  , interfaceLoadAction = \_ iface -> pure iface
  , pluginRecompile     = purePlugin
  }
HS

cat > "$plug_src/TestHoleFitPlugin.hs" <<'HS'
{-# LANGUAGE Haskell2010 #-}
module TestHoleFitPlugin (plugin) where

import GHC.Plugins
import GHC.Tc.Utils.Monad (newTcRef)

plugin :: Plugin
plugin = defaultPlugin
  { holeFitPlugin = \_ -> Just $ HoleFitPluginR
      { hfPluginInit = newTcRef ()
      , hfPluginRun  = \_ -> HoleFitPlugin
          { candPlugin = \_ cs -> pure cs
          , fitPlugin  = \_ fs -> pure (RawHoleFit (text "pluginSynthesizedFit") : fs)
          }
      , hfPluginStop = \_ -> pure ()
      }
  , pluginRecompile = purePlugin
  }
HS

cat > "$plug_src/TestCoreInspectPlugin.hs" <<'HS'
{-# LANGUAGE Haskell2010 #-}
module TestCoreInspectPlugin (plugin) where

import GHC.Plugins

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = \opts todos -> pure (CoreDoPluginPass "InspectDependentCore" (inspect opts) : todos)
  , pluginRecompile  = purePlugin
  }
  where
    inspect opts guts = do
      case opts of
        path : _ -> liftIO $ appendFile path
          ("core:" ++ moduleNameString (moduleName (mg_module guts))
           ++ ":" ++ show (length (flattenBinds (mg_binds guts))) ++ "\n")
        [] -> pure ()
      pure guts
HS

cat > "$plug_src/TestCoreMutatePlugin.hs" <<'HS'
{-# LANGUAGE Haskell2010 #-}
module TestCoreMutatePlugin (plugin) where

import GHC.Plugins

-- Mutates 'substSingle' or 'flipConst' Core RHSs incompatibly by replacing
-- constructor/helper occurrences or returning its argument unchanged so
-- post-optimization equation validators in GHC.Driver.Main.Passes reject the
-- broken equation!
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = \_ todos -> pure ( todos ++ [CoreDoPluginPass "MutateCore" mutate] )
  , pluginRecompile  = purePlugin
  }
  where
    mutate guts = pure (guts { mg_binds = map mutBind (mg_binds guts) })
    mutBind (NonRec b rhs) = NonRec b (mutRhs b rhs)
    mutBind (Rec pairs)    = Rec [(b, mutRhs b rhs) | (b, rhs) <- pairs]

    -- For substSingle :: forall g b a. Term (CExt g b) a -> Term g b -> Term g a:
    -- rewrite (\ @g @b @a m v -> ...) when a ~ b or for monoTarget :: Bool -> Bool -> Bool:
    -- rewrite (\ x y -> ...) to (\ x _ -> x)!
    mutRhs b rhs
      | occNameString (getOccName b) == "eqNat"
      , (bndrs, _body) <- collectBinders rhs
      , [x, y] <- filter isId bndrs
      = mkLams bndrs (Var x `seq` Var y `seq` Var falseDataConId)
      | occNameString (getOccName b) == "lookupEnvBad"
      , (bndrs, _body) <- collectBinders rhs
      , v_last : _ <- reverse (filter isId bndrs)
      , (_, _) <- (v_last, rhs)
      = let swapAlt (Alt con bs e) = Alt con bs (swapBoolExpr e)
            walk (Lam v e) = Lam v (walk e)
            walk (Case s cb ty alts) = Case s cb ty (map swapAlt alts)
            walk e = swapBoolExpr e
        in walk rhs
      | otherwise = rhs

    swapBoolExpr (Var v)
      | v == trueDataConId  = Var falseDataConId
      | v == falseDataConId = Var trueDataConId
    swapBoolExpr (App f a) = App (swapBoolExpr f) (swapBoolExpr a)
    swapBoolExpr (Cast e c) = Cast (swapBoolExpr e) c
    swapBoolExpr e = e
HS

cat > "$plug_src/TestLatePlugin.hs" <<'HS'
{-# LANGUAGE Haskell2010 #-}
module TestLatePlugin (plugin) where

import GHC.Plugins

plugin :: Plugin
plugin = defaultPlugin
  { latePlugin      = \_ _ x -> pure x
  , pluginRecompile = purePlugin
  }
HS

unit_id="test-general-plugins-0.1"
ghc_ver=$("$ghc_bin" "-B$libdir" --numeric-version)
so_name="libHS${unit_id}-ghc${ghc_ver}.so"

"${base_ghc[@]}" -O2 -this-unit-id "$unit_id" -package ghc \
  -i"$plug_src" -odir "$plug_lib" -hidir "$plug_lib" \
  -c "$plug_src/TestTcPlugin.hs" \
     "$plug_src/TestSourcePlugin.hs" \
     "$plug_src/TestHoleFitPlugin.hs" \
     "$plug_src/TestCoreInspectPlugin.hs" \
     "$plug_src/TestCoreMutatePlugin.hs" \
     "$plug_src/TestLatePlugin.hs"

"${base_ghc[@]}" -O2 -dynamic -fPIC -osuf dyn_o -hisuf dyn_hi \
  -this-unit-id "$unit_id" -package ghc \
  -i"$plug_src" -odir "$plug_lib" -hidir "$plug_lib" \
  -c "$plug_src/TestTcPlugin.hs" \
     "$plug_src/TestSourcePlugin.hs" \
     "$plug_src/TestHoleFitPlugin.hs" \
     "$plug_src/TestCoreInspectPlugin.hs" \
     "$plug_src/TestCoreMutatePlugin.hs" \
     "$plug_src/TestLatePlugin.hs"

ar rcs "$plug_lib/libHS${unit_id}.a" \
  "$plug_lib/TestTcPlugin.o" \
  "$plug_lib/TestSourcePlugin.o" \
  "$plug_lib/TestHoleFitPlugin.o" \
  "$plug_lib/TestCoreInspectPlugin.o" \
  "$plug_lib/TestCoreMutatePlugin.o" \
  "$plug_lib/TestLatePlugin.o"

"${base_ghc[@]}" -shared -dynamic -no-hs-main -package ghc \
  "-optl-Wl,-soname,$so_name" \
  "$plug_lib/TestTcPlugin.dyn_o" \
  "$plug_lib/TestSourcePlugin.dyn_o" \
  "$plug_lib/TestHoleFitPlugin.dyn_o" \
  "$plug_lib/TestCoreInspectPlugin.dyn_o" \
  "$plug_lib/TestCoreMutatePlugin.dyn_o" \
  "$plug_lib/TestLatePlugin.dyn_o" \
  -o "$plug_lib/$so_name"

ghc_dep_id=$("$ghc_pkg" --package-db "$pkg_db" field ghc id --simple-output | head -n1)
base_dep_id=$("$ghc_pkg" --package-db "$pkg_db" field base id --simple-output | head -n1)

cat <<EOF | "$ghc_pkg" --package-db "$pkg_db" register - >/dev/null
name: test-general-plugins
version: 0.1
id: $unit_id
key: $unit_id
exposed: True
exposed-modules: TestTcPlugin TestSourcePlugin TestHoleFitPlugin TestCoreInspectPlugin TestCoreMutatePlugin TestLatePlugin
import-dirs: $plug_lib
library-dirs: $plug_lib
dynamic-library-dirs: $plug_lib
hs-libraries: HS${unit_id}
depends: $base_dep_id $ghc_dep_id
EOF

export LD_LIBRARY_PATH="$plug_lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

# 3. Shared multi-module Dependent Haskell program (using Data.Nat.Dependent + local
#    SourceTable + SourceClosure substSingle + native DepFunTy foreach proofs)
mkdir -p "$out/prog-src"
cat > "$out/prog-src/DepProg.hs" <<'HS'
{-# LANGUAGE DependentTypes, LambdaCase, ListTuplePuns #-}
module Main (main) where

import Data.Type.Equality ((:~:) (Refl))
import Data.Nat.Dependent (Nat(..), eqSize, natToInt, plus)
import Data.Nat.Dependent.Properties (eqSizeRefl, plusZero)
import Prelude (Bool(..), Eq(..), IO, error, pure)

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

substLaw :: forall g a (v :: Term g a). substSingle (Var Z) v :~: v
substLaw = Refl

checkNat :: foreach (n :: Nat) -> (plus n Zero :~: n, eqSize n n :~: True)
checkNat n = (plusZero n, eqSizeRefl n)

main :: IO ()
main = do
  let n2 = Succ (Succ Zero)
  case (checkNat n2, substLaw @CEmpty @(TArr TNat TNat) @(Lam (Var Z))) of
    ((Refl, Refl), Refl) ->
      if natToInt (plus n2 Zero) == 2
        then pure ()
        else error "DepProg mismatch"
HS

# 4. Run ALL FOUR plugin categories across --make (-fno-code, -O0, -O2)!
for plug in TestTcPlugin TestSourcePlugin TestHoleFitPlugin TestCoreInspectPlugin; do
  # 4a. -fno-code -fwrite-interface --make
  nc_dir="$out/matrix/$plug/nocode"
  mkdir -p "$nc_dir"
  trace_file="$nc_dir/plugin.trace"
  : > "$trace_file"
  sed 's/module Main (main) where/module DepProg (main) where/' "$out/prog-src/DepProg.hs" > "$nc_dir/DepProg.hs"
  "${base_ghc[@]}" --make -fno-code -fwrite-interface \
    -package ghc-experimental -package ghc-internal -XGHC2024 \
    -package test-general-plugins "-fplugin=$plug" "-fplugin-opt=$plug:$trace_file" \
    -i -i"$dep_base_src" -i"$open_eval_dir" -i"$nc_dir" \
    -odir "$nc_dir/build" -hidir "$nc_dir/build" \
    "$nc_dir/DepProg.hs"

  case "$plug" in
    TestTcPlugin)          grep -Fq 'tc-init' "$trace_file" ;;
    TestSourcePlugin)      grep -Fq 'parsed:DepProg' "$trace_file"
                           grep -Fq 'typechecked:Data.Nat.Dependent' "$trace_file" ;;
    TestCoreInspectPlugin) grep -Fq 'core:Data.Nat.Dependent:' "$trace_file"
                           grep -Fq 'core:DepProg:' "$trace_file" ;;
  esac

  # 4b. Full native -O0 and -O2 --make compilation + execution
  for opt in -O0 -O2; do
    cg_dir="$out/matrix/$plug/$opt"
    mkdir -p "$cg_dir"
    cg_trace="$cg_dir/plugin.trace"
    : > "$cg_trace"
    "${base_ghc[@]}" "$opt" -dynamic --make -dcore-lint -dstg-lint \
      -package ghc-experimental -package ghc-internal -XGHC2024 \
      -package test-general-plugins "-fplugin=$plug" "-fplugin-opt=$plug:$cg_trace" \
      -i -i"$dep_base_src" -i"$open_eval_dir" -i"$out/prog-src" \
      -odir "$cg_dir/build" -hidir "$cg_dir/build" \
      "$out/prog-src/DepProg.hs" -o "$cg_dir/dep-prog"
    "$cg_dir/dep-prog"

    case "$plug" in
      TestTcPlugin)          grep -Fq 'tc-init' "$cg_trace" ;;
      TestSourcePlugin)      grep -Fq 'parsed:Main' "$cg_trace"
                             grep -Fq 'typechecked:Data.Nat.Dependent' "$cg_trace" ;;
      TestCoreInspectPlugin) grep -Fq 'core:Data.Nat.Dependent:' "$cg_trace"
                             grep -Fq 'core:Main:' "$cg_trace" ;;
    esac
  done
done

# 5. Typed-hole check with ALL FOUR plugins active simultaneously on a module
#    that defines its own promoted/closure functions AND holes!
mkdir -p "$out/holes"
cat > "$out/holes/ProofWithHoles.hs" <<'HS'
{-# LANGUAGE DependentTypes, LambdaCase #-}
module ProofWithHoles where

import Data.Type.Equality ((:~:) (Refl))
import Data.Nat.Dependent (Nat(..), eqSize, plus)
import Data.Nat.Dependent.Properties (eqSizeRefl, plusZero)

localDouble :: Nat -> Nat
localDouble Zero     = Zero
localDouble (Succ n) = Succ (Succ (localDouble n))

holeLocal :: localDouble (Succ Zero) :~: Succ (Succ Zero)
holeLocal = _

holeForeach :: foreach (m :: Nat) -> plus m Zero :~: m
holeForeach m = _

useImportedEqSize :: foreach (n :: Nat) -> eqSize n n :~: True
useImportedEqSize n = eqSizeRefl n
HS

if "${base_ghc[@]}" --make -fno-code -fwrite-interface \
     -package ghc-experimental -package ghc-internal -XGHC2024 \
     -package test-general-plugins \
     -fplugin=TestTcPlugin \
     -fplugin=TestSourcePlugin \
     -fplugin=TestHoleFitPlugin \
     -fplugin=TestCoreInspectPlugin \
     -i -i"$dep_base_src" -i"$open_eval_dir" -i"$out/holes" \
     -odir "$out/holes/build" -hidir "$out/holes/build" \
     "$out/holes/ProofWithHoles.hs" >"$out/holes/holes.log" 2>&1; then
  echo "Expected typed-hole failure in ProofWithHoles.hs" >&2
  exit 1
fi
grep -Fq 'pluginSynthesizedFit' "$out/holes/holes.log"
grep -Fq 'Refl ::' "$out/holes/holes.log"
grep -Fq 'plusZero m ::' "$out/holes/holes.log"

# 6. Negative controls!
#    6a. TestCoreMutatePlugin mutates Core RHSs incompatibly -> Passes.hs catches
#        both early-indexed lookup mismatches ([GHC-68947] on same module) AND
#        FinalCoreSourceTable mismatches ([GHC-70565] on client) in BOTH codegen and -fno-code!
mkdir -p "$out/neg-mutate"
cat > "$out/neg-mutate/BadLookup.hs" <<'HS'
{-# LANGUAGE DependentTypes #-}
module BadLookup (Key(..), Env(..), lookupEnvBad, law) where

import Data.Type.Equality ((:~:) (Refl))
import Prelude (Bool(..))

data Key = K0 | K1
data Env where
  ENil  :: Env
  ECons :: Bool -> Env -> Env

lookupEnvBad :: Env -> Key -> Bool
lookupEnvBad ENil        _  = False
lookupEnvBad (ECons b _) K0 = b
lookupEnvBad (ECons _ e) K1 = lookupEnvBad e K0

law :: lookupEnvBad ENil K0 :~: False
law = Refl
HS

for extra in "" "-fno-code -fwrite-interface"; do
  read -ra ex_flags <<<"$extra"
  rm -rf "$out/neg-mutate/build1"
  mkdir -p "$out/neg-mutate/build1"
  if "${base_ghc[@]}" -O0 -dynamic "${ex_flags[@]}" -c \
       -package test-general-plugins -fplugin=TestCoreMutatePlugin \
       -odir "$out/neg-mutate/build1" -hidir "$out/neg-mutate/build1" \
       "$out/neg-mutate/BadLookup.hs" >"$out/neg-mutate/lookup.log" 2>&1 ||
     ! grep -Fq '[GHC-68947]' "$out/neg-mutate/lookup.log"; then
    echo "Expected GHC-68947 when Core plugin mutates lookupEnvBad ($extra)" >&2
    cat "$out/neg-mutate/lookup.log" >&2
    exit 1
  fi
done

cat > "$out/neg-mutate/MutatedProvider.hs" <<'HS'
{-# LANGUAGE DependentTypes #-}
module MutatedProvider (Nat(..), eqNat) where
import Prelude (Bool(..))

data Nat = Zero | Succ Nat

eqNat :: Nat -> Nat -> Bool
eqNat Zero     Zero     = True
eqNat Zero     (Succ _) = False
eqNat (Succ _) Zero     = False
eqNat (Succ m) (Succ n) = eqNat m n
HS

cat > "$out/neg-mutate/MutatedClient.hs" <<'HS'
{-# LANGUAGE DependentTypes #-}
module MutatedClient where
import Data.Type.Equality ((:~:) (Refl))
import MutatedProvider (Nat(..), eqNat)
import Prelude (Bool(..))

badProof :: foreach (n :: Nat) -> eqNat n n :~: True
badProof Zero     = Refl
badProof (Succ n) = badProof n
HS

rm -rf "$out/neg-mutate/build-ok"
mkdir -p "$out/neg-mutate/build-ok"
"${base_ghc[@]}" --make -O0 -fno-code -fwrite-interface \
  -package test-general-plugins -fplugin=TestCoreInspectPlugin \
  -i -i"$out/neg-mutate" \
  -odir "$out/neg-mutate/build-ok" -hidir "$out/neg-mutate/build-ok" \
  "$out/neg-mutate/MutatedClient.hs"

for extra in "-dynamic" "-fno-code -fwrite-interface"; do
  read -ra ex_flags <<<"$extra"
  rm -rf "$out/neg-mutate/build2"
  mkdir -p "$out/neg-mutate/build2"
  if "${base_ghc[@]}" --make -O0 "${ex_flags[@]}" \
       -package test-general-plugins -fplugin=TestCoreMutatePlugin \
       -i -i"$out/neg-mutate" \
       -odir "$out/neg-mutate/build2" -hidir "$out/neg-mutate/build2" \
       "$out/neg-mutate/MutatedClient.hs" >"$out/neg-mutate/final.log" 2>&1 ||
     ! grep -Eq '\[GHC-(70565|68947)\]' "$out/neg-mutate/final.log"; then
    echo "Expected equation withdrawal when Core plugin mutates eqNat ($extra)" >&2
    cat "$out/neg-mutate/final.log" >&2
    exit 1
  fi
done

#    6b. TestLatePlugin (active latePlugin running after .hi creation) still rejects
#        native dependent Core cleanly!
mkdir -p "$out/neg-late"
cat > "$out/neg-late/NativeLate.hs" <<'HS'
{-# LANGUAGE DependentTypes #-}
module NativeLate where
import Data.Type.Equality ((:~:) (Refl))
data Nat = Zero | Succ Nat
reflNat :: foreach (n :: Nat) -> n :~: n
reflNat _ = Refl
HS
if "${base_ghc[@]}" -O0 -dynamic -c \
     -package test-general-plugins -fplugin=TestLatePlugin \
     -odir "$out/neg-late" -hidir "$out/neg-late" \
     "$out/neg-late/NativeLate.hs" >"$out/neg-late/late.log" 2>&1 ||
   ! grep -Fq 'Native dependent Core cannot enter late plugins' "$out/neg-late/late.log"; then
  echo "Expected late plugins rejection with TestLatePlugin" >&2
  cat "$out/neg-late/late.log" >&2
  exit 1
fi
