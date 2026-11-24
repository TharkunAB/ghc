#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
trap 'echo "NativeCheckedClosure.sh failed at line $LINENO: $BASH_COMMAND" >&2' ERR

inputs=$PWD
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fno-write-if-simplified-core -fdiagnostics-color=never)

for opt in -O0 -O2; do
    provider="$work/$opt/provider"
    mkdir -p "$provider" "$work/$opt"/{native,no-code,withheld}
    cd "$provider"
    cp "$inputs/NativeCheckedClosureProvider.hs" .
    "${ghc[@]}" "$opt" -fwrite-source-equations -fexpose-all-unfoldings -c NativeCheckedClosureProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedClosureProvider.hi >provider.iface
    grep -Fq 'CheckedClosureSourceTable T:replace' provider.iface
    grep -Fq 'SourceTable T:findMarked' provider.iface
    grep -Fq 'SourceTable T:markFound' provider.iface

    for mode in native no-code; do
        cd "$work/$opt/$mode"
        cp "$provider/NativeCheckedClosureProvider.hi" "$inputs/NativeCheckedClosure.hs" .
        args=()
        if [[ $mode == no-code ]]; then args=(-fno-code -fwrite-interface); fi
        "${ghc[@]}" "$opt" "${args[@]}" -DCHECK_INDEXED_RECURSION -c NativeCheckedClosure.hs
        test ! -f NativeCheckedClosureProvider.o
        if [[ $mode == no-code ]]; then test ! -f NativeCheckedClosure.o; fi
    done
    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -DWRONG_CAPTURE -c NativeCheckedClosure.hs >wrong.log 2>&1; then
        cat wrong.log
        exit 1
    fi
    grep -Fq '[GHC-83865]' wrong.log

    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -DCHECK_INDEXED_RECURSION -DWRONG_STAMP \
         -c NativeCheckedClosure.hs >wrong.log 2>&1; then
        cat wrong.log
        exit 1
    fi
    grep -Fq 'other' wrong.log

    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -DCHECK_INDEXED_RECURSION -DRULE_MARKED \
         -c NativeCheckedClosure.hs >rule.log 2>&1; then
        cat rule.log
        exit 1
    fi
    grep -Fq '[GHC-83605]' rule.log
    grep -Fq 'P.findMarked' rule.log

    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -DCHECK_INDEXED_RECURSION -DWRONG_DUPLICATE \
         -c NativeCheckedClosure.hs >duplicate.log 2>&1; then
        exit 1
    fi
    grep -Eq '\[GHC-(83865|25897)\]' duplicate.log
    grep -Fq InTail duplicate.log

    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -DCHECK_INDEXED_RECURSION -DRULE_ENCODER \
         -c NativeCheckedClosure.hs >rule-encoder.log 2>&1; then
        cat rule-encoder.log
        exit 1
    fi
    grep -Fq '[GHC-83605]' rule-encoder.log
    grep -Fq 'P.markFound' rule-encoder.log

    cd "$work/$opt/withheld"
    cp "$inputs/NativeCheckedClosureProvider.hs" .
    "${ghc[@]}" "$opt" -fno-write-source-equations -fomit-interface-pragmas -c NativeCheckedClosureProvider.hs
    cp "$inputs/NativeCheckedClosure.hs" .
    if "${ghc[@]}" "$opt" -fno-code -c NativeCheckedClosure.hs >open.log 2>&1; then
        exit 1
    fi
    grep -Fq '[GHC-70565]' open.log
    grep -Fq 'This argument is a type variable. Closed evaluation requires a known value.' open.log

    for variant in STRICT_CAPTURE HIDE_LOOP; do
        "${ghc[@]}" "$opt" -fwrite-source-equations -fexpose-all-unfoldings \
            -fforce-recomp "-D$variant" -c NativeCheckedClosureProvider.hs
        "${ghc[@]}" --show-iface NativeCheckedClosureProvider.hi >withheld.iface
        if grep -Eq 'CheckedClosureSourceTable T:replace|\$apply:replace' withheld.iface; then
            exit 1
        fi
    done
    for variant in STRICT_TAIL CHANGED_STAMP HIDE_MARKED; do
        "${ghc[@]}" "$opt" -fwrite-source-equations -fexpose-all-unfoldings \
            -fforce-recomp "-D$variant" -c NativeCheckedClosureProvider.hs
        "${ghc[@]}" --show-iface NativeCheckedClosureProvider.hi >withheld.iface
        if grep -Fq 'SourceTable T:findMarked' withheld.iface; then
            exit 1
        fi
    done
    for variant in STRICT_ENCODE HIDE_MARKED; do
        "${ghc[@]}" "$opt" -fwrite-source-equations -fexpose-all-unfoldings \
            -fforce-recomp "-D$variant" -c NativeCheckedClosureProvider.hs
        "${ghc[@]}" --show-iface NativeCheckedClosureProvider.hi >withheld.iface
        if grep -Fq 'SourceTable T:markFound' withheld.iface; then
            exit 1
        fi
    done
done

for opt in -O0 -O2; do
    mkdir -p "$work/$opt/double"/{provider,reader}
    cd "$work/$opt/double/provider"
    cp "$inputs/NativeCheckedClosureDoubleProvider.hs" .
    "${ghc[@]}" "$opt" -fwrite-source-equations -c NativeCheckedClosureDoubleProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedClosureDoubleProvider.hi >provider.iface
    grep -Fq 'CheckedClosureSourceTable T:replaceBoth' provider.iface
    grep -Fq 'CheckedClosureSourceTable T:renameExpr [extendName]' provider.iface
    grep -Fq 'SourceTable T:extendName' provider.iface

    cd "$work/$opt/double/reader"
    cp ../provider/NativeCheckedClosureDoubleProvider.hi "$inputs/NativeCheckedClosureDouble.hs" .
    "${ghc[@]}" "$opt" -c NativeCheckedClosureDouble.hs
    "${ghc[@]}" "$opt" -DABSTRACT_RENAMING -fno-code -c NativeCheckedClosureDouble.hs
    if "${ghc[@]}" "$opt" -DABSTRACT_RENAMING -DWRONG_RENAMING -fno-code \
         -c NativeCheckedClosureDouble.hs >wrong.log 2>&1; then
        cat wrong.log
        exit 1
    fi
    grep -Fq '[GHC-83865]' wrong.log
    if "${ghc[@]}" "$opt" -DWRONG_DOUBLE_CAPTURE -c NativeCheckedClosureDouble.hs >wrong.log 2>&1; then
        cat wrong.log
        exit 1
    fi
    grep -Fq '[GHC-83865]' wrong.log

    if [[ $opt == -O2 ]]; then
        cp "$inputs/NativeCheckedClosureDoubleRule.hs" .
        "${ghc[@]}" "$opt" -c NativeCheckedClosureDoubleRule.hs
        if "${ghc[@]}" "$opt" -DHELPER_RULE -c NativeCheckedClosureDouble.hs >rule.log 2>&1; then
            cat rule.log
            exit 1
        fi
        grep -Fq '[GHC-83605]' rule.log
        # Keep the deliberately conflicting rule visible in the interface.
        "${ghc[@]}" "$opt" -Wno-inline-rule-shadowing -fforce-recomp -DABSTRACT_RENAMING \
             -c NativeCheckedClosureDoubleRule.hs
        if "${ghc[@]}" "$opt" -DHELPER_RULE -DABSTRACT_RENAMING -fno-code \
             -c NativeCheckedClosureDouble.hs >rule.log 2>&1; then
            cat rule.log
            exit 1
        fi
        grep -Fq '[GHC-83605]' rule.log
        grep -Fq 'extendName' rule.log
    fi

    mkdir -p "$work/$opt/subst"/{provider,reader}
    cd "$work/$opt/subst/provider"
    cp "$inputs/NativeCheckedClosureDoubleProvider.hs" .
    "${ghc[@]}" "$opt" -DNAMED_WEAKENING -fwrite-source-equations \
        -c NativeCheckedClosureDoubleProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedClosureDoubleProvider.hi >provider.iface
    grep -Fq 'SourceTable T:weakenSlot' provider.iface
    grep -Fq 'CheckedClosureSourceTable T:extendExpr' provider.iface
    grep -Fq 'CheckedClosureSourceTable T:walk' provider.iface

    cd "$work/$opt/subst/reader"
    cp ../provider/NativeCheckedClosureDoubleProvider.hi "$inputs/NativeCheckedClosureDouble.hs" .
    "${ghc[@]}" "$opt" -DABSTRACT_SUBST -c NativeCheckedClosureDouble.hs
    test ! -e NativeCheckedClosureDoubleProvider.o
    if "${ghc[@]}" "$opt" -fforce-recomp -DABSTRACT_SUBST -DWRONG_SUBST_EXT \
         -c NativeCheckedClosureDouble.hs >wrong-ext.log 2>&1; then
        cat wrong-ext.log
        exit 1
    fi
    grep -Fq '[GHC-05617]' wrong-ext.log
    if "${ghc[@]}" "$opt" -fforce-recomp -DABSTRACT_SUBST -DWRONG_SUBST \
         -c NativeCheckedClosureDouble.hs >wrong-subst.log 2>&1; then
        cat wrong-subst.log
        exit 1
    fi
    grep -Fq '[GHC-83865]' wrong-subst.log

    for rule_flag in LOCAL_EXT_RULE LOCAL_WALK_RULE; do
        if "${ghc[@]}" "$opt" -fforce-recomp -fenable-rewrite-rules -Wno-inline-rule-shadowing \
             -DABSTRACT_SUBST_EQ "-D$rule_flag" -c NativeCheckedClosureDouble.hs >local-rule.log 2>&1; then
            cat local-rule.log
            exit 1
        fi
        grep -Fq '[GHC-83605]' local-rule.log
    done

    if [[ $opt == -O2 ]]; then
        cp "$inputs/NativeCheckedClosureDoubleRule.hs" .
        for pair in "SUBST_EXT_RULE:extendExpr" "SUBST_WALK_RULE:walk"; do
            flag=${pair%%:*}
            expected=${pair#*:}
            "${ghc[@]}" "$opt" -Wno-inline-rule-shadowing -fforce-recomp "-D$flag" \
                -c NativeCheckedClosureDoubleRule.hs
            if "${ghc[@]}" "$opt" -fforce-recomp -DHELPER_RULE -DABSTRACT_SUBST \
                 -c NativeCheckedClosureDouble.hs >subst-rule.log 2>&1; then
                cat subst-rule.log
                exit 1
            fi
            grep -Fq '[GHC-83605]' subst-rule.log
            grep -Fq "$expected" subst-rule.log
        done
    fi

    cd "$work/$opt/subst/provider"
    "${ghc[@]}" "$opt" -fforce-recomp -DNAMED_WEAKENING -DEXTRA_EXT_HELPER \
        -fwrite-source-equations -c NativeCheckedClosureDoubleProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedClosureDoubleProvider.hi >withheld.iface
    if grep -Eq 'CheckedClosureSourceTable T:(extendExpr|walk)' withheld.iface; then
        cat withheld.iface
        exit 1
    fi
done

mkdir "$work/interactive"
cd "$work/interactive"
cp "$inputs/NativeCheckedClosureProvider.hs" "$inputs/NativeCheckedClosure.hs" .
"${ghc[@]}" --interactive -ignore-dot-ghci -fwrite-source-equations \
    NativeCheckedClosure.hs -e 'print caseBinder' >observed
grep -Fxq Refl observed
test ! -e NativeCheckedClosureProvider.o

mkdir "$work/interactive-double"
cd "$work/interactive-double"
cp "$inputs/"{NativeCheckedClosureDoubleProvider.hs,NativeCheckedClosureDouble.hs} .
"${ghc[@]}" --interactive -ignore-dot-ghci -fwrite-source-equations \
    NativeCheckedClosureDouble.hs -e 'print underBoth' >observed
grep -Fxq Refl observed
test ! -e NativeCheckedClosureDoubleProvider.o
if "${ghc[@]}" --interactive -ignore-dot-ghci -fforce-recomp -fwrite-source-equations \
       -DWRONG_DOUBLE_CAPTURE NativeCheckedClosureDouble.hs -e '()' >wrong.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' wrong.log; then
    cat wrong.log >&2
    exit 1
fi

# Check the shared index on a symbolic pair of operands through a native interface.
for opt in -O0 -O2; do
    mkdir -p "$work/$opt/lookup"/{provider,reader}
    cd "$work/$opt/lookup/provider"
    cp "$inputs/NativeCheckedLookupProvider.hs" .
    "${ghc[@]}" "$opt" -fwrite-source-equations -fomit-interface-pragmas \
        -c NativeCheckedLookupProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedLookupProvider.hi >provider.iface
    grep -Fq 'SourceTable T:lookupEnv' provider.iface
    grep -Fq 'T:lookupEnv field2 field3' provider.iface

    cd ../reader
    cp ../provider/NativeCheckedLookupProvider.hi "$inputs/NativeCheckedLookup.hs" .
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c NativeCheckedLookup.hs
    test ! -e NativeCheckedLookup.o
    "${ghc[@]}" "$opt" -fforce-recomp -c NativeCheckedLookup.hs
    "${ghc[@]}" "$opt" NativeCheckedLookup.o ../provider/NativeCheckedLookupProvider.o -o run
    test "$(./run)" = '(True,True,True)'

    if "${ghc[@]}" "$opt" -fforce-recomp -DWRONG_INDEX -c NativeCheckedLookup.hs >wrong.log 2>&1 ||
       ! grep -Fq '[GHC-25897]' wrong.log; then
        cat wrong.log
        exit 1
    fi
    if "${ghc[@]}" "$opt" -fforce-recomp -DACTIVE_RULE -fenable-rewrite-rules \
         -c NativeCheckedLookup.hs >rule.log 2>&1 || ! grep -Fq '[GHC-83605]' rule.log; then
        cat rule.log
        exit 1
    fi

    cd ../provider
    "${ghc[@]}" "$opt" -fforce-recomp -fwrite-source-equations -DWRONG_BODY \
        -c NativeCheckedLookupProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedLookupProvider.hi >provider.iface
    if grep -Fq 'SourceTable T:lookupEnv' provider.iface; then
        echo "Unexpected SourceTable T:lookupEnv for -DWRONG_BODY ($opt)" >&2
        exit 1
    fi

    "${ghc[@]}" "$opt" -fforce-recomp -fwrite-source-equations -DHIDE_ENV \
        -c NativeCheckedLookupProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedLookupProvider.hi >provider.iface
    if grep -Fq 'SourceTable T:lookupEnv' provider.iface; then
        echo "Unexpected SourceTable T:lookupEnv for -DHIDE_ENV ($opt)" >&2
        exit 1
    fi
    cp NativeCheckedLookupProvider.hi ../reader/
    cd ../reader
    cat >HiddenEnvClient.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module HiddenEnvClient where
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeCheckedLookupProvider as P
bad :: forall (n :: P.Nat) (e :: P.Env n) (i :: P.Fin n). P.lookupEnv e i :~: 'True
bad = Refl
HS
    if "${ghc[@]}" "$opt" -fforce-recomp -fno-code -c HiddenEnvClient.hs >hidden-env.log 2>&1 ||
       ! grep -Eq '\[GHC-(70565|68947)\]' hidden-env.log; then
        echo "Unexpected result for HiddenEnvClient.hs ($opt):" >&2
        cat hidden-env.log >&2
        exit 1
    fi
done

# Check same-module mutual environment/closure + GADT lookup, declaration/clause orders,
# constructor visibility, and early-vs-final Core agreement at -O0 and -O2.
for opt in -O0 -O2; do
    for variant in STANDARD REVERSE_DECLS REVERSE_CLAUSES BOTH_REVERSED EXPLICIT_FORALL; do
        dir="$work/$opt/local-lookup/$variant"
        mkdir -p "$dir"/{provider,nocode,reader}
        extra=()
        case "$variant" in
            REVERSE_DECLS) extra=(-DREVERSE_DECLS) ;;
            REVERSE_CLAUSES) extra=(-DREVERSE_CLAUSES) ;;
            BOTH_REVERSED) extra=(-DREVERSE_DECLS -DREVERSE_CLAUSES) ;;
            EXPLICIT_FORALL) extra=(-DEXPLICIT_FORALL) ;;
        esac

        cd "$dir/nocode"
        cp "$inputs/NativeLocalLookup.hs" .
        "${ghc[@]}" "$opt" -fno-code -fwrite-interface -fomit-interface-pragmas \
            "${extra[@]}" -c NativeLocalLookup.hs
        test -s NativeLocalLookup.hi && test ! -e NativeLocalLookup.o

        cd "$dir/provider"
        cp "$inputs/NativeLocalLookup.hs" .
        "${ghc[@]}" "$opt" -fomit-interface-pragmas "${extra[@]}" -c NativeLocalLookup.hs
        "${ghc[@]}" --show-iface NativeLocalLookup.hi >local.iface
        grep -Fq 'SourceTable T:lookupEnv' local.iface
        if grep -Eq '\(lookupEnv :: .*->' local.iface; then
            cat local.iface >&2
            exit 1
        fi

        cd "$dir/reader"
        cp ../provider/NativeLocalLookup.hi "$inputs/NativeLocalLookupClient.hs" .
        test ! -e NativeLocalLookup.hs
        "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c NativeLocalLookupClient.hs
        test ! -e NativeLocalLookupClient.o
        "${ghc[@]}" "$opt" -fforce-recomp -c NativeLocalLookupClient.hs
        "${ghc[@]}" "$opt" NativeLocalLookupClient.o ../provider/NativeLocalLookup.o -o run
        test "$(./run)" = '(True,True,True,True)'
    done

    ctrl="$work/$opt/local-lookup/controls"
    mkdir -p "$ctrl"/{provider,abstract-eval,abstract-reader,unused-splice}
    cd "$ctrl/provider"
    cp "$inputs/NativeLocalLookup.hs" .

    reject_local() {
        local expected=$1
        shift
        if "${ghc[@]}" "$opt" -fforce-recomp "$@" -c NativeLocalLookup.hs >reject.log 2>&1; then
            echo "Unexpectedly compiled NativeLocalLookup.hs with flags: $*" >&2
            exit 1
        fi
        if ! grep -Fq "[$expected]" reject.log; then
            echo "Expected [$expected] for flags: $*" >&2
            cat reject.log >&2
            exit 1
        fi
    }

    reject_local GHC-25897 -DWRONG_INDEX
    reject_local GHC-68947 -DWRONG_BODY
    reject_local GHC-83605 -DLOCAL_RULE
    reject_local GHC-68947 -DSPECIALISE_LOOKUP
    reject_local GHC-70566 -DHIDE_ENV_CONS
    grep -Fq 'Export the constructors of Env' reject.log
    reject_local GHC-70566 -DHIDE_FIN_CONS
    grep -Fq 'Export the constructors of Fin' reject.log
    reject_local GHC-70566 -DHIDE_CLOS_CONS
    grep -Fq 'Export the constructors of Clos' reject.log
    reject_local GHC-70566 -DHIDE_LOOKUP
    grep -Fq 'Export lookupEnv' reject.log

    # Early-vs-final Core disagreement: coverage ticks (-fhpc) alter Core after early
    # staging succeeds and must not leave early equations silently certified.
    reject_local GHC-68947 -fhpc
    reject_local GHC-68947 -fno-code -fwrite-interface -fhpc

    # Single-constructor product result types still preserve lazy demand and pass both stages.
    "${ghc[@]}" "$opt" -fforce-recomp -DSINGLE_PRODUCT_CLOS -c NativeLocalLookup.hs
    "${ghc[@]}" "$opt" -fforce-recomp -fno-code -fwrite-interface -DSINGLE_PRODUCT_CLOS -c NativeLocalLookup.hs

    # An imported orphan rule via hs-boot must also invalidate same-module staged lookup at -O0 and -O2.
    mkdir -p "$ctrl/orphan-rule"
    cd "$ctrl/orphan-rule"
    cp "$inputs/NativeLocalLookup.hs" .
    cat >NativeLocalLookup.hs-boot <<'BOOT'
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module NativeLocalLookup (Nat(..), Fin(..), UTerm(..), Env(..), Clos(..), lookupEnv) where
data Nat = Zero | Succ Nat
data Fin (n :: Nat) where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)
data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm ('Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n
data Env (n :: Nat) where
  ENil  :: Env 'Zero
  ECons :: Clos -> Env n -> Env ('Succ n)
data Clos where
  MkClos :: UTerm n -> Env n -> Clos
lookupEnv :: Env n -> Fin n -> Clos
BOOT
    cat >OrphanLookupRule.hs <<'ORPHAN'
{-# OPTIONS_GHC -Wno-orphans #-}
module OrphanLookupRule () where
import {-# SOURCE #-} NativeLocalLookup
{-# RULES "orphan lookup hijack" forall c e. lookupEnv (ECons c e) FZ = MkClos (ULam (UVar FZ)) ENil #-}
ORPHAN
    "${ghc[@]}" "$opt" -c NativeLocalLookup.hs-boot
    "${ghc[@]}" "$opt" -fenable-rewrite-rules -fno-omit-interface-pragmas -c OrphanLookupRule.hs
    if "${ghc[@]}" "$opt" -fno-ignore-interface-pragmas -fforce-recomp -DIMPORTED_ORPHAN_RULE \
         -c NativeLocalLookup.hs >orphan.log 2>&1 ||
       ! grep -Fq '[GHC-83605]' orphan.log; then
        echo "Unexpected result for -DIMPORTED_ORPHAN_RULE ($opt):" >&2
        cat orphan.log >&2
        exit 1
    fi

    # Exporting EvalBig abstractly (without EVar) while exporting Env(..), Fin(..), Clos(..),
    # lookupEnv allows lookupEnv in client types while keeping EVar hidden.
    cd "$ctrl/abstract-eval"
    cp "$inputs/NativeLocalLookup.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -DABSTRACT_EVAL -c NativeLocalLookup.hs
    cp NativeLocalLookup.hi "$ctrl/abstract-reader/"
    cd "$ctrl/abstract-reader"
    cp "$inputs/NativeLocalLookupClient.hs" .
    "${ghc[@]}" "$opt" -DABSTRACT_CLIENT -c NativeLocalLookupClient.hs
    "${ghc[@]}" "$opt" NativeLocalLookupClient.o ../abstract-eval/NativeLocalLookup.o -o run
    test "$(./run)" = '(True,True,True)'
    if "${ghc[@]}" "$opt" -fforce-recomp -DABSTRACT_CLIENT -DUSE_HIDDEN_EVAR \
         -c NativeLocalLookupClient.hs >hidden-evar.log 2>&1 ||
       ! grep -Fq '[GHC-76037]' hidden-evar.log; then
        echo "Unexpected result for -DUSE_HIDDEN_EVAR ($opt):" >&2
        cat hidden-evar.log >&2
        exit 1
    fi

    # An unused lookup before a TH splice with abstract Env compiles cleanly without leaking T:lookupEnv.
    cd "$ctrl/unused-splice"
    cp "$inputs/NativeLocalLookup.hs" .
    "${ghc[@]}" "$opt" -fno-write-source-equations -DSPLICE_UNUSED_ABSTRACT -c NativeLocalLookup.hs
    "${ghc[@]}" --show-iface NativeLocalLookup.hi >unused.iface
    if grep -Fq 'T:lookupEnv' unused.iface; then
        echo "Unexpected T:lookupEnv in unused-splice interface ($opt):" >&2
        cat unused.iface >&2
        exit 1
    fi
done

# Fresh -fno-code providers, unexported helper/constructor controls, and upstream
# -fno-code / -fomit-interface-pragmas rule heads at -O0 and -O2.
for opt in -O0 -O2; do
    nc_dir="$work/$opt/fresh-nocode"
    mkdir -p "$nc_dir"/{provider,reader,withheld,hidden-helper,rule-nocode,rule-omit,orphan-nocode}

    # 1. Fresh -fno-code provider publishes checked closure and indexed tables.
    cd "$nc_dir/provider"
    cp "$inputs/NativeCheckedClosureProvider.hs" .
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -fomit-interface-pragmas \
        -fwrite-source-equations -c NativeCheckedClosureProvider.hs
    test ! -e NativeCheckedClosureProvider.o
    "${ghc[@]}" --show-iface NativeCheckedClosureProvider.hi >provider.iface
    grep -Fq 'CheckedClosureSourceTable T:replace' provider.iface
    grep -Fq 'SourceTable T:markFound' provider.iface
    if [[ $opt == -O0 ]]; then
        grep -Fq 'SourceTable T:findMarked' provider.iface
    fi

    cd "$nc_dir/reader"
    cp ../provider/NativeCheckedClosureProvider.hi "$inputs/NativeCheckedClosure.hs" .
    if [[ $opt == -O0 ]]; then
        "${ghc[@]}" "$opt" -fno-code -fwrite-interface -DCHECK_INDEXED_RECURSION -c NativeCheckedClosure.hs
    else
        "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c NativeCheckedClosure.hs
    fi
    test ! -e NativeCheckedClosure.o
    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -DWRONG_CAPTURE -c NativeCheckedClosure.hs >wrong.log 2>&1; then
        cat wrong.log >&2
        exit 1
    fi
    grep -Fq '[GHC-83865]' wrong.log

    # 1b. Fresh -fno-code plug/substU provider publishes tables and HIDE_CTX withholds T:plug (GHC-70565).
    mkdir -p "$nc_dir/plug-nocode"/{prov,cli}
    cd "$nc_dir/plug-nocode/prov"
    cp "$inputs/NativeCheckedPlugSubstUProvider.hs" .
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -fomit-interface-pragmas \
        -fwrite-source-equations -c NativeCheckedPlugSubstUProvider.hs
    test ! -e NativeCheckedPlugSubstUProvider.o
    "${ghc[@]}" --show-iface NativeCheckedPlugSubstUProvider.hi >plug.iface
    for needle in 'SourceTable T:plug' 'SourceTable T:plugS' \
                  'CheckedClosureSourceTable T:substSingleU' \
                  'CheckedClosureSourceTable T:renameU [extU]' \
                  'SourceTable T:extU'; do
        grep -Fq "$needle" plug.iface || { echo "Missing $needle in -fno-code at $opt" >&2; exit 1; }
    done
    cd "$nc_dir/plug-nocode/cli"
    cp ../prov/NativeCheckedPlugSubstUProvider.hi "$inputs/NativeCheckedPlugSubstU.hs" .
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c NativeCheckedPlugSubstU.hs
    test ! -e NativeCheckedPlugSubstU.o

    cd "$nc_dir/plug-nocode/prov"
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -fomit-interface-pragmas \
        -fwrite-source-equations -fforce-recomp -DHIDE_CTX -c NativeCheckedPlugSubstUProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedPlugSubstUProvider.hi >plug-hide.iface
    if grep -Eq 'SourceTable T:plug([^S]|$)' plug-hide.iface; then
        echo "Unexpectedly published T:plug under -fno-code -DHIDE_CTX ($opt)" >&2
        exit 1
    fi
    cd "$nc_dir/plug-nocode/cli"
    cp ../prov/NativeCheckedPlugSubstUProvider.hi .
    cat >HiddenCtxClient.hs <<'HCC'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module HiddenCtxClient where
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeCheckedPlugSubstUProvider as P
bad :: forall (n :: P.Nat) (t :: P.UTerm n). P.plug 'P.CHole t :~: t
bad = Refl
HCC
    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -c HiddenCtxClient.hs >hctx.log 2>&1 ||
       ! grep -Fq '[GHC-70566]' hctx.log; then
        echo "Expected [GHC-70566] for HiddenCtxClient.hs ($opt):" >&2
        cat hctx.log >&2
        exit 1
    fi
    cat >HiddenCtxVarClient.hs <<'HCVC'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module HiddenCtxVarClient where
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeCheckedPlugSubstUProvider as P
bad :: forall (c :: P.UCtx ('P.Succ 'P.Zero) 'P.Zero) (t :: P.UTerm ('P.Succ 'P.Zero)).
       P.plug c t :~: 'P.UVar 'P.FZ
bad = Refl
HCVC
    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -c HiddenCtxVarClient.hs >hctx-var.log 2>&1 ||
       ! grep -Fq '[GHC-70565]' hctx-var.log; then
        echo "Expected [GHC-70565] for HiddenCtxVarClient.hs under -DHIDE_CTX ($opt):" >&2
        cat hctx-var.log >&2
        exit 1
    fi

    # 2. HIDE_LOOP, HIDE_MARKED, STRICT_CAPTURE withhold publication in -fno-code without .hi crashes.
    cd "$nc_dir/withheld"
    cp "$inputs/NativeCheckedClosureProvider.hs" .
    for variant in STRICT_CAPTURE HIDE_LOOP; do
        "${ghc[@]}" "$opt" -fno-code -fwrite-interface -fomit-interface-pragmas \
            -fwrite-source-equations -fforce-recomp "-D$variant" -c NativeCheckedClosureProvider.hs
        test ! -e NativeCheckedClosureProvider.o
        "${ghc[@]}" --show-iface NativeCheckedClosureProvider.hi >withheld.iface
        if grep -Eq 'CheckedClosureSourceTable T:replace|\$apply:replace' withheld.iface; then
            echo "Unexpectedly published T:replace under -fno-code -D$variant ($opt)" >&2
            exit 1
        fi
    done
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -fomit-interface-pragmas \
        -fwrite-source-equations -fforce-recomp -DHIDE_MARKED -c NativeCheckedClosureProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedClosureProvider.hi >withheld.iface
    if grep -Eq 'SourceTable T:(findMarked|markFound)' withheld.iface; then
        echo "Unexpectedly published marked tables under -fno-code -DHIDE_MARKED ($opt)" >&2
        exit 1
    fi

    # 3. Unexported helper in -fno-code must withhold publication and fail client with GHC-70565.
    cd "$nc_dir/hidden-helper"
    cat >HiddenHelperProvider.hs <<'HH'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase, PolyKinds, RankNTypes #-}
module HiddenHelperProvider
  ( Sort(..), Scope(..), Slot(..), Expr(..), substSingle ) where
data Sort = Nat | Arrow Sort Sort
data Scope = Empty | Push Scope Sort
data Slot (g :: Scope) (a :: Sort) where
  New :: Slot (Push g a) a
  Old :: Slot g a -> Slot (Push g b) a
data Expr (g :: Scope) (a :: Sort) where
  At   :: Slot g a -> Expr g a
  Bind :: Expr (Push g a) b -> Expr g (Arrow a b)
  Lit  :: Expr g Nat
extendName :: (forall a. Slot g a -> Slot d a) -> (forall s. Slot (Push g b) s -> Slot (Push d b) s)
extendName _ New = New
extendName f (Old x) = Old (f x)
renameExpr :: (forall a. Slot g a -> Slot d a) -> Expr g s -> Expr d s
renameExpr f (At x) = At (f x)
renameExpr f (Bind n) = Bind (renameExpr (extendName f) n)
renameExpr _ Lit = Lit
extendExpr :: (forall a. Slot g a -> Expr d a) -> (forall s. Slot (Push g b) s -> Expr (Push d b) s)
extendExpr _ New = At New
extendExpr f (Old x) = renameExpr Old (f x)
walk :: (forall a. Slot g a -> Expr d a) -> Expr g s -> Expr d s
walk f (At x) = f x
walk f (Bind n) = Bind (walk (extendExpr f) n)
walk _ Lit = Lit
substSingle :: Expr (Push g a) b -> Expr g a -> Expr g b
substSingle n w = walk (\case { New -> w; Old x -> At x }) n
HH
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -fomit-interface-pragmas \
        -fwrite-source-equations -c HiddenHelperProvider.hs
    test ! -e HiddenHelperProvider.o
    "${ghc[@]}" --show-iface HiddenHelperProvider.hi >hh.iface
    if grep -Fq 'CheckedClosureSourceTable T:substSingle' hh.iface; then
        echo "Unexpectedly published T:substSingle with hidden helpers ($opt)" >&2
        exit 1
    fi
    cat >HiddenHelperClient.hs <<'HHC'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module HiddenHelperClient where
import Data.Type.Equality ((:~:)(Refl))
import qualified HiddenHelperProvider as P
bad :: P.substSingle ('P.At 'P.New :: P.Expr ('P.Push 'P.Empty 'P.Nat) 'P.Nat) 'P.Lit :~: 'P.Lit
bad = Refl
HHC
    if "${ghc[@]}" "$opt" -fno-code -c HiddenHelperClient.hs >hh-client.log 2>&1 ||
       ! grep -Fq '[GHC-70565]' hh-client.log; then
        echo "Expected [GHC-70565] for HiddenHelperClient.hs ($opt):" >&2
        cat hh-client.log >&2
        exit 1
    fi

    # 4. Upstream M_Rule compiled with -fno-code -fwrite-interface -fomit-interface-pragmas
    #    (and native -fomit-interface-pragmas) must withhold substSingle at both -O0 and -O2.
    for rule_kind in nocode omit; do
        rdir="$nc_dir/rule-$rule_kind"
        cd "$rdir"
        cat >RuleClosureProvider.hs-boot <<'RBOOT'
{-# LANGUAGE DataKinds, GADTs, PolyKinds, RankNTypes #-}
module RuleClosureProvider
  ( Sort(..), Scope(..), Slot(..), Expr(..), substSingle ) where
data Sort = Nat | Arrow Sort Sort
data Scope = Empty | Push Scope Sort
data Slot (g :: Scope) (a :: Sort) where
  New :: Slot (Push g a) a
  Old :: Slot g a -> Slot (Push g b) a
data Expr (g :: Scope) (a :: Sort) where
  At   :: Slot g a -> Expr g a
  Bind :: Expr (Push g a) b -> Expr g (Arrow a b)
  Lit  :: Expr g Nat
substSingle :: Expr (Push g a) b -> Expr g a -> Expr g b
RBOOT
        cat >M_Rule.hs <<'MRULE'
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}
module M_Rule () where
import {-# SOURCE #-} RuleClosureProvider
{-# RULES "hijack" forall (n :: Expr (Push g 'Nat) 'Nat) (w :: Expr g 'Nat). substSingle n w = Lit #-}
MRULE
        cat >RuleClosureProvider.hs <<'RPROV'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase, PolyKinds, RankNTypes #-}
module RuleClosureProvider
  ( Sort(..), Scope(..), Slot(..), Expr(..)
  , extendName, renameExpr, extendExpr, walk, substSingle ) where
import M_Rule ()
data Sort = Nat | Arrow Sort Sort
data Scope = Empty | Push Scope Sort
data Slot (g :: Scope) (a :: Sort) where
  New :: Slot (Push g a) a
  Old :: Slot g a -> Slot (Push g b) a
data Expr (g :: Scope) (a :: Sort) where
  At   :: Slot g a -> Expr g a
  Bind :: Expr (Push g a) b -> Expr g (Arrow a b)
  Lit  :: Expr g Nat
extendName :: (forall a. Slot g a -> Slot d a) -> (forall s. Slot (Push g b) s -> Slot (Push d b) s)
extendName _ New = New
extendName f (Old x) = Old (f x)
renameExpr :: (forall a. Slot g a -> Slot d a) -> Expr g s -> Expr d s
renameExpr f (At x) = At (f x)
renameExpr f (Bind n) = Bind (renameExpr (extendName f) n)
renameExpr _ Lit = Lit
extendExpr :: (forall a. Slot g a -> Expr d a) -> (forall s. Slot (Push g b) s -> Expr (Push d b) s)
extendExpr _ New = At New
extendExpr f (Old x) = renameExpr Old (f x)
walk :: (forall a. Slot g a -> Expr d a) -> Expr g s -> Expr d s
walk f (At x) = f x
walk f (Bind n) = Bind (walk (extendExpr f) n)
walk _ Lit = Lit
substSingle :: Expr (Push g a) b -> Expr g a -> Expr g b
substSingle n w = walk (\case { New -> w; Old x -> At x }) n
RPROV
        cat >RuleClosureClient.hs <<'RCLI'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module RuleClosureClient where
import Data.Type.Equality ((:~:)(Refl))
import qualified RuleClosureProvider as P
bad :: P.substSingle ('P.At 'P.New :: P.Expr ('P.Push 'P.Empty 'P.Nat) 'P.Nat) 'P.Lit :~: 'P.Lit
bad = Refl
RCLI
        "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c RuleClosureProvider.hs-boot
        if [[ $rule_kind == nocode ]]; then
            "${ghc[@]}" "$opt" -fno-code -fwrite-interface -fomit-interface-pragmas -c M_Rule.hs
            test ! -e M_Rule.o
        else
            "${ghc[@]}" "$opt" -fomit-interface-pragmas -c M_Rule.hs
        fi
        for prov_mode in nocode native; do
            prov_flags=()
            if [[ $prov_mode == nocode ]]; then prov_flags=(-fno-code -fwrite-interface); fi
            "${ghc[@]}" "$opt" "${prov_flags[@]}" -fomit-interface-pragmas \
                -fwrite-source-equations -fforce-recomp -c RuleClosureProvider.hs
            "${ghc[@]}" --show-iface RuleClosureProvider.hi >rprov.iface
            if grep -Fq 'CheckedClosureSourceTable T:substSingle' rprov.iface; then
                echo "Unexpectedly published T:substSingle ($opt $rule_kind $prov_mode)" >&2
                exit 1
            fi
            if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -c RuleClosureClient.hs >rcli.log 2>&1 ||
               ! grep -Fq '[GHC-70565]' rcli.log; then
                echo "Expected [GHC-70565] for RuleClosureClient ($opt $rule_kind $prov_mode):" >&2
                cat rcli.log >&2
                exit 1
            fi
        done
    done

    # 5. Same-module indexed lookup importing -fno-code -fomit-interface-pragmas orphan rule
    #    must reject with GHC-83605 at both -O0 and -O2 even without -fno-ignore-interface-pragmas.
    cd "$nc_dir/orphan-nocode"
    cp "$inputs/NativeLocalLookup.hs" .
    cat >NativeLocalLookup.hs-boot <<'OBOOT'
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module NativeLocalLookup (Nat(..), Fin(..), UTerm(..), Env(..), Clos(..), lookupEnv) where
data Nat = Zero | Succ Nat
data Fin (n :: Nat) where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)
data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm ('Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n
data Env (n :: Nat) where
  ENil  :: Env 'Zero
  ECons :: Clos -> Env n -> Env ('Succ n)
data Clos where
  MkClos :: UTerm n -> Env n -> Clos
lookupEnv :: Env n -> Fin n -> Clos
OBOOT
    cat >OrphanLookupRule.hs <<'ORULE'
{-# OPTIONS_GHC -Wno-orphans #-}
module OrphanLookupRule () where
import {-# SOURCE #-} NativeLocalLookup
{-# RULES "orphan lookup hijack" forall c e. lookupEnv (ECons c e) FZ = MkClos (ULam (UVar FZ)) ENil #-}
ORULE
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c NativeLocalLookup.hs-boot
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -fomit-interface-pragmas -c OrphanLookupRule.hs
    for lmode in nocode native; do
        lflags=()
        if [[ $lmode == nocode ]]; then lflags=(-fno-code -fwrite-interface); fi
        if "${ghc[@]}" "$opt" "${lflags[@]}" -fforce-recomp -DIMPORTED_ORPHAN_RULE \
             -c NativeLocalLookup.hs >orphan.log 2>&1 ||
           ! grep -Fq '[GHC-83605]' orphan.log; then
            echo "Expected [GHC-83605] for -DIMPORTED_ORPHAN_RULE ($opt $lmode):" >&2
            cat orphan.log >&2
            exit 1
        fi
    done
done
