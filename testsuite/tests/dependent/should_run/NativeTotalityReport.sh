#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
trap 'echo "NativeTotalityReport.sh failed at line $LINENO: $BASH_COMMAND" >&2' ERR

inputs=$PWD
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

for opt in -O0 -O2; do
    dir="$work/$opt"
    mkdir -p "$dir"/{report,plain}

    # 1. Compile with -freport-totality (and -Wno-incomplete-patterns -Werror to verify
    #    advisory MCInfo messages never trigger -Werror failure!) and capture stderr.
    cd "$dir/report"
    cp "$inputs/NativeTotalityReport.hs" .
    "${ghc[@]}" "$opt" -freport-totality -Wno-incomplete-patterns -Werror \
        NativeTotalityReport.hs -o run >stdout.log 2>totality.log

    # Positive structural totality classifications:
    grep -Fq 'totality: plus: checked-structural' totality.log
    grep -Fq 'totality: appendVec: checked-structural' totality.log
    grep -Fq 'totality: lookupVec: checked-structural' totality.log
    grep -Fq 'totality: congSucc: checked-structural' totality.log
    grep -Fq 'totality: plusAssoc: checked-structural' totality.log
    grep -Fq 'totality: stepHelper: checked-structural' totality.log
    grep -Fq 'totality: callsCheckedHelper: checked-structural' totality.log

    # Negative / conservative unknown classifications with exact reasons:
    grep -Fq 'totality: divergentHelper: unknown (non-structural recursive call)' totality.log
    grep -Fq 'totality: callsDivergentHelper: unknown (calls non-total local function)' totality.log
    grep -Fq 'totality: loop: unknown (non-structural recursive call)' totality.log
    grep -Fq 'totality: apply: unknown (higher-order call)' totality.log
    grep -Fq 'totality: headOnly: unknown (partial or guarded equations)' totality.log
    grep -Fq 'totality: revExternal: unknown (imports external function)' totality.log

    actual=$(./run)
    [[ "$actual" == "totality-advisory-ok" ]]

    # 2. Default compilation (without -freport-totality) must emit zero totality lines
    #    and produce identical runtime behavior.
    cd "$dir/plain"
    cp "$inputs/NativeTotalityReport.hs" .
    "${ghc[@]}" "$opt" -Wno-incomplete-patterns NativeTotalityReport.hs -o run >plain.out 2>plain.err
    if grep -Fq 'totality:' plain.err; then
        echo "Unexpected totality output when -freport-totality is off" >&2
        cat plain.err >&2
        exit 1
    fi
    [[ "$(./run)" == "totality-advisory-ok" ]]
done

# 3. Verify mutual recursion is conservatively classified as unknown (mutual recursion).
mkdir -p "$work/mutual"
cd "$work/mutual"
cat >MutualCheck.hs <<'HS'
module MutualCheck (evenN, oddN) where

data Nat = Zero | Succ Nat

evenN :: Nat -> Bool
evenN Zero     = True
evenN (Succ n) = oddN n

oddN :: Nat -> Bool
oddN Zero     = False
oddN (Succ n) = evenN n
HS
"${ghc[@]}" -O0 -fno-code -freport-totality -Werror -c MutualCheck.hs 2>mutual.log
grep -Fq 'totality: evenN: unknown (mutual recursion)' mutual.log
grep -Fq 'totality: oddN: unknown (mutual recursion)' mutual.log
