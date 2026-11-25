#!/usr/bin/env bash
set -euo pipefail
docs=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
exec bash "$docs/check-dependent-base.sh" "$@"
