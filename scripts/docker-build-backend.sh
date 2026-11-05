#!/bin/sh
# Both BuildKit cache mounts must be held with sharing=locked for this script.
set -eu
work_dir="${LOTR_BUILD_WORK_DIR:-/build/.stack-work}"
output_dir="${LOTR_BUILD_OUTPUT_DIR:-/out}"
dirty="$work_dir/.docker-build-in-progress"
mkdir -p "$work_dir"
set --

if [ -f "$dirty" ]; then
  echo 'Checking artifacts after an interrupted build (keeping completed modules)'
  python3 "$(dirname "$0")/recover-stack-cache.py" "$work_dir" "$dirty"
  # Archives/install outputs are package-wide. Let Cabal recreate them; do not
  # remove complete per-module objects or unrelated dependency caches.
  find "$work_dir" -type f -newer "$dirty" \
    \( -name '*.a' -o -name '*.so' -o -name '*.tmp' \) -exec rm -f {} +
  for executable in "$work_dir"/dist/*/ghc-*/build/LordOfTheRings-exe; do
    [ ! -d "$executable" ] || rm -f "$executable/LordOfTheRings-exe"
  done
  set -- --force-dirty
fi

touch "$dirty"
if [ "${LOTR_BUILD_TIMINGS:-0}" = 1 ]; then
  set -- "$@" --ghc-options '-ddump-timings +RTS -s -RTS'
fi
# Keep the marker on ANY failure, including a killed compiler or lost build client.
# Recovery validates pairs on the next invocation, rather than clearing the cache.
stack install --no-terminal "$@" --local-bin-path "$output_dir" LordOfTheRings:exe:LordOfTheRings-exe
rm -f "$dirty"
