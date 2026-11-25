#!/usr/bin/env bash
set -euo pipefail

comp_dir=$(cd "$(dirname "$1")" && pwd)
if [[ -d "$comp_dir/package" ]]; then
    export LD_LIBRARY_PATH="$comp_dir/package${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
fi

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -fdiagnostics-color=never -ferror-spans)
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

mkdir -p "$out/driver"
cat > "$out/driver/AuditDriver.hs" <<'HS'
module Main (main) where

import GHC hiding (typeKind)
import GHC.Driver.Env (hsc_plugins)
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Plugins (Plugin (..), PluginWithArgs (..), StaticPlugin (..), defaultPlugin, purePlugin)
import GHC.Plugins hiding ((<>))
import GHC.Core.TyCo.Rep (Type (..), Coercion (..), MCoercion (..))
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Coercion (coVarTypesRole)
import Control.Monad (unless, when)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  [libdir, optFlag, stamp, srcFile, outBin] <- getArgs
  ok <- runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    let rawFlags = map noLoc
          [ optFlag
          , "-v0"
          , "-dcore-lint"
          , "-dstg-lint"
          , "-fforce-recomp"
          , "-o"
          , outBin
          ]
    logger <- getLogger
    (dflags1, _, _) <- parseDynamicFlags logger dflags0 rawFlags
    _ <- setSessionDynFlags dflags1
    let sp = StaticPlugin
          { spPlugin = PluginWithArgs auditPlugin [stamp]
          , spInitialised = False
          }
    modifySession $ \env ->
      env { hsc_plugins = (hsc_plugins env) { staticPlugins = [sp] } }
    target <- guessTarget srcFile Nothing Nothing
    setTargets [target]
    load LoadAllTargets
  case ok of
    Succeeded -> pure ()
    Failed -> exitFailure

auditPlugin :: Plugin
auditPlugin = defaultPlugin
  { installCoreToDos = install
  , pluginRecompile  = purePlugin
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos =
  let stamp = case opts of
        (p : _) -> p
        []      -> "audit.stamp"
      early = CoreDoPluginPass "AuditHomogeneousBindersEarly" (auditPass "early" stamp)
      late  = CoreDoPluginPass "AuditHomogeneousBindersLate"  (auditPass "late"  stamp)
  in pure (early : todos ++ [late])

auditPass :: String -> FilePath -> ModGuts -> CoreM ModGuts
auditPass phase stamp guts = do
  let (covar_count, bad) = foldMap auditBind (mg_binds guts)
  unless (null bad) $
    pprPanic ("Mixed-kind CoVar binder in " ++ phase) (vcat bad)
  when (phase == "late") $
    liftIO $ writeFile stamp (show (getSum covar_count) ++ "\n")
  pure guts

newtype SumInt = SumInt { getSum :: Int }

instance Semigroup SumInt where
  SumInt a <> SumInt b = SumInt (a + b)

instance Monoid SumInt where
  mempty = SumInt 0

auditVar :: Var -> (SumInt, [SDoc])
auditVar v =
  let self
        | isCoVar v =
            let (lhs, rhs, _) = coVarTypesRole v
                ok = typeKind lhs `eqType` typeKind rhs
            in (SumInt 1, [ppr v <+> dcolon <+> ppr (varType v) | not ok])
        | otherwise = mempty
  in self <> auditType (varType v)

auditBind :: CoreBind -> (SumInt, [SDoc])
auditBind (NonRec b rhs) = auditVar b <> auditExpr rhs
auditBind (Rec pairs) = foldMap (\(b, rhs) -> auditVar b <> auditExpr rhs) pairs

auditExpr :: CoreExpr -> (SumInt, [SDoc])
auditExpr (Var v) = auditType (varType v)
auditExpr (Lit _) = mempty
auditExpr (App f a) = auditExpr f <> auditExpr a
auditExpr (Lam b e) = auditVar b <> auditExpr e
auditExpr (Let bind body) = auditBind bind <> auditExpr body
auditExpr (Case s b ty alts) =
  auditExpr s <> auditVar b <> auditType ty <> foldMap auditAlt alts
auditExpr (Cast e co) = auditExpr e <> auditCo co
auditExpr (Tick _ e) = auditExpr e
auditExpr (Type ty) = auditType ty
auditExpr (Coercion co) = auditCo co
auditExpr (Pack s w e) = auditType s <> auditType w <> auditExpr e
auditExpr (Unpack w p pkg body) =
  auditVar w <> auditVar p <> auditExpr pkg <> auditExpr body

auditAlt :: CoreAlt -> (SumInt, [SDoc])
auditAlt (Alt _ bs rhs) = foldMap auditVar bs <> auditExpr rhs

auditType :: Type -> (SumInt, [SDoc])
auditType (TyVarTy _) = mempty
auditType (AppTy f a) = auditType f <> auditType a
auditType (TyConApp _ args) = foldMap auditType args
auditType (ForAllTy ma (Bndr b _) body) =
  auditType ma <> auditVar b <> auditType body
auditType (FunTy _ ma mult arg res) =
  auditType ma <> auditType mult <> auditType arg <> auditType res
auditType (LitTy _) = mempty
auditType (CastTy ty co) = auditType ty <> auditCo co
auditType (CoercionTy co) = auditCo co
auditType (ExistsTy _ b body) = auditVar b <> auditType body
auditType (DepFunTy _ b body) = auditVar b <> auditType body
auditType (DepDictTy b body) = auditVar b <> auditType body
auditType (TermVarTy _) = mempty
auditType (TermAppTy _ arg) = auditType arg
auditType (ClassAppTy _ inst d a k) =
  auditType inst <> auditType d <> auditType a <> auditType k

auditCo :: Coercion -> (SumInt, [SDoc])
auditCo (Refl ty) = auditType ty
auditCo (GRefl _ ty mco) = auditType ty <> auditMCo mco
auditCo (TyConAppCo _ _ cos) = foldMap auditCo cos
auditCo (AppCo c1 c2) = auditCo c1 <> auditCo c2
auditCo (ForAllCo { fco_tcv = b, fco_ma = ma, fco_kind = kco, fco_body = body }) =
  auditVar b <> auditCo ma <> auditMCo kco <> auditCo body
auditCo (FunCo { fco_ma = ma, fco_mult = mult, fco_arg = c1, fco_res = c2 }) =
  auditCo ma <> auditCo mult <> auditCo c1 <> auditCo c2
auditCo (CoVarCo _) = mempty
auditCo (AxiomCo _ cos) = foldMap auditCo cos
auditCo (UnivCo { uco_lty = t1, uco_rty = t2 }) = auditType t1 <> auditType t2
auditCo (SymCo c) = auditCo c
auditCo (TransCo c1 c2) = auditCo c1 <> auditCo c2
auditCo (SelCo _ c) = auditCo c
auditCo (LRCo _ c) = auditCo c
auditCo (InstCo c1 c2) = auditCo c1 <> auditCo c2
auditCo (KindCo c) = auditCo c
auditCo (SubCo c) = auditCo c
auditCo (HoleCo _) = mempty
auditCo (DepFunCo _ _ b dom body) =
  auditVar b <> auditMCo dom <> auditCo body
auditCo (TermAppCo _ c) = auditCo c

auditMCo :: MCoercion -> (SumInt, [SDoc])
auditMCo MRefl = mempty
auditMCo (MCo c) = auditCo c
HS

if ! "${ghc[@]}" -O0 -fPIC -dynamic -package ghc \
    -outputdir "$out/driver" \
    "$out/driver/AuditDriver.hs" -o "$out/driver/audit-driver" \
    >"$out/driver/build.log" 2>&1; then
    cat "$out/driver/build.log" >&2
    exit 1
fi

for opt in -O0 -O2; do
    mkdir -p "$out/run$opt"
    cd "$out/run$opt"
    cp "$src/NativeHomogeneousGadtBinders.hs" .
    stamp="$out/run$opt/audit.stamp"
    "$out/driver/audit-driver" "$2" "$opt" "$stamp" \
        "$out/run$opt/NativeHomogeneousGadtBinders.hs" \
        "$out/run$opt/run-audit"
    test -s "$stamp"
    count=$(cat "$stamp")
    if (( count <= 0 )); then
        echo "Expected audited CoVar binders at $opt, got $count" >&2
        exit 1
    fi
    "$out/run$opt/run-audit"
done
