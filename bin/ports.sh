#!/usr/bin/env bash

_PORTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ROOT="${ROOT:-$_PORTS_DIR}"

if [ -n "${SUTRA_OFFSET:-}" ]; then
  :
else
  _h="$(printf '%s' "$ROOT" | cksum | cut -d' ' -f1)"
  SUTRA_OFFSET=$(( _h % 250 + 1 ))
fi
export SUTRA_OFFSET

export SUTRA_NREPL_PORT=$(( 7888 + SUTRA_OFFSET ))
export SUTRA_HTTP_PORT=$(( 8030 + SUTRA_OFFSET ))
export SUTRA_SHADOW_HTTP_PORT=$(( 9630 + SUTRA_OFFSET ))
export SUTRA_SHADOW_NREPL_PORT=$(( 9000 + SUTRA_OFFSET ))
