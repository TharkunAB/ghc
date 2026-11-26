#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

usage() {
    echo "Usage: bash $0 GHC LIBDIR" >&2
    exit 2
}
[[ $# -eq 2 ]] || usage
compiler=$1
libdir=$2

docs=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$docs/../.." && pwd)
dep_base=${DEPENDENT_BASE_DIR:-"$repo_root/../dependent-base"}
if [[ ! -d "$dep_base/src" ]]; then
    echo "dependent-base source tree not found at $dep_base (set DEPENDENT_BASE_DIR)" >&2
    exit 1
fi

out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

ghc=("$compiler" "-B$libdir" -v0 -package-env - -dcore-lint -dstg-lint -fdiagnostics-color=never
     -package base -package ghc-experimental -package ghc-internal)

mapfile -t lib_srcs < <(find "$dep_base/src" -name '*.hs' | sort)

for opt in -O0 -O2; do
    dir="$out/$opt"
    mkdir -p "$dir"/{lib,test,iface-client}

    # 1. Compile all dependent-base library modules into a clean interface/object directory.
    "${ghc[@]}" "$opt" -XGHC2024 -Wall \
        -i "-i$dep_base/src" \
        -outputdir "$dir/lib" --make -no-link \
        "${lib_srcs[@]}"

    # 2. Verify that an ordinary Haskell2010 client compiles against .hi files alone
    #    (no dependent-base .hs sources on the search path and no -XDependentTypes).
    (cd "$dir/lib" && find . -name '*.hi' -exec cp --parents -t "$dir/iface-client" {} +)
    cp "$dep_base/test/OrdinaryInteropClient.hs" "$dir/iface-client/"
    (
        cd "$dir/iface-client"
        "${ghc[@]}" "$opt" -XHaskell2010 "-i$dir/iface-client" -i. -c OrdinaryInteropClient.hs
    )

    # 3. Build and run the full dependent-base test suite (DependentClient + OrdinaryInteropClient + Main).
    "${ghc[@]}" "$opt" -XGHC2024 -Wall \
        -i "-i$dep_base/src" "-i$dep_base/test" \
        -outputdir "$dir/test" --make "$dep_base/test/Main.hs" -o "$dir/run-dependent-base"
    "$dir/run-dependent-base" >"$dir/stdout"
done

diff -u "$out/-O0/stdout" "$out/-O2/stdout"
