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
open_eval="$docs/examples/open-evaluation"
[[ -d "$open_eval" ]] || open_eval="$docs/open-evaluation"
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

ghc=("$compiler" "-B$libdir" -v0 -package-env - -dcore-lint -dstg-lint -fdiagnostics-color=never)

for opt in -O0 -O2; do
    dir="$out/$opt"
    mkdir -p "$dir"/{lib,test,iface-client}

    # 1. Compile all HaskLib library modules into a clean interface/object directory.
    "${ghc[@]}" "$opt" -XGHC2024 \
        -i "-i$docs/hasklib" "-i$open_eval" \
        -outputdir "$dir/lib" --make -no-link \
        "$docs/hasklib/HaskLib.hs" \
        "$docs/hasklib/HaskLib/Equality.hs" \
        "$docs/hasklib/HaskLib/Fin.hs" \
        "$docs/hasklib/HaskLib/List.hs" \
        "$docs/hasklib/HaskLib/Nat.hs" \
        "$docs/hasklib/HaskLib/Sigma.hs" \
        "$docs/hasklib/HaskLib/Size.hs" \
        "$docs/hasklib/HaskLib/Vec.hs"

    # 2. Verify that an ordinary Haskell2010 client compiles against .hi files alone
    #    (no HaskLib .hs sources on the search path and no -XDependentTypes).
    (cd "$dir/lib" && find . -name '*.hi' -exec cp --parents -t "$dir/iface-client" {} +)
    cp "$docs/hasklib-test/OrdinaryInteropClient.hs" "$dir/iface-client/"
    (
        cd "$dir/iface-client"
        "${ghc[@]}" "$opt" -XHaskell2010 "-i$dir/iface-client" -i. -c OrdinaryInteropClient.hs
    )

    # 3. Build and run the full HaskLib test suite (dependent Client + Haskell2010 OrdinaryInteropClient + Main).
    "${ghc[@]}" "$opt" -XGHC2024 \
        -i "-i$docs/hasklib" "-i$open_eval" "-i$docs/hasklib-test" \
        -outputdir "$dir/test" --make "$docs/hasklib-test/Main.hs" -o "$dir/run-hasklib"
    "$dir/run-hasklib" >"$dir/stdout"
done

diff -u "$out/-O0/stdout" "$out/-O2/stdout"
