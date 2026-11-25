#!/usr/bin/env bash
set -euo pipefail
here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
trap 'echo "NativeHaskLibInterop.sh failed at line $LINENO: $BASH_COMMAND" >&2' ERR

if [[ -f check-dependent-base.sh ]]; then
    exec bash check-dependent-base.sh "$1" "$2"
elif [[ -f check-hasklib.sh ]]; then
    exec bash check-hasklib.sh "$1" "$2"
else
    exec bash "$here/../../../../docs/dependent-haskell/check-dependent-base.sh" "$1" "$2"
fi
