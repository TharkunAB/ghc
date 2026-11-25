#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
trap 'echo "NativeHaskLibInterop.sh failed at line $LINENO: $BASH_COMMAND" >&2' ERR

if [[ -f check-hasklib.sh ]]; then
    exec bash check-hasklib.sh "$1" "$2"
else
    here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
    exec bash "$here/../../../../docs/dependent-haskell/check-hasklib.sh" "$1" "$2"
fi
