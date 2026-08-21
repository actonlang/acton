#!/usr/bin/env bash
# Measure the attribute-lookup typechecker test (see README.md).
#
# Usage:
#   ./run.sh [gen.py options]
#
# Environment:
#   ACTON           compiler to measure (default: ../../dist/bin/acton)
#   ACTON_BASELINE  optional second compiler, measured for comparison
#   WORK            work directory (default: a fresh mktemp -d, printed at exit)
#
# Examples:
#   ./run.sh
#   ./run.sh --lists 40
#   ACTON_BASELINE=/path/to/older/dist/bin/acton ./run.sh
#
# The metric is the typecheck time of the consumer module on a rebuild after
# a comment-only edit. This is the edit-recompile cycle of a module that
# imports a large closure of generated schema modules. The schema modules are
# already compiled, and the compiler reads them from .tydb.
set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
ACTON="${ACTON:-$DIR/../../dist/bin/acton}"
ACTON_BASELINE="${ACTON_BASELINE:-}"
WORK="${WORK:-$(mktemp -d -t attr_lookup_repro)}"

measure() {
    local bin=$1 label=$2
    local proj=$WORK/$label t0 t1
    python3 "$DIR/gen.py" --out "$proj" "${@:3}" >/dev/null
    t0=$SECONDS
    if ! (cd "$proj" && "$bin" build --skip-build --no-progress >"$proj/build.log" 2>&1); then
        echo "$label: initial build FAILED (see $proj/build.log)" >&2
        return 1
    fi
    t1=$SECONDS
    echo "$label: full build (all modules)      $((t1 - t0)) s"
    echo "# rebuild" >>"$proj/src/consume.act"
    (cd "$proj" && "$bin" build --skip-build --no-progress 2>&1) \
        | grep "consume.*Type check done" \
        | sed "s/^ *consume *Type check done */$label: consume rebuild typecheck   /"
}

echo "acton: $ACTON"
measure "$ACTON" current "$@"
if [ -n "$ACTON_BASELINE" ]; then
    echo "baseline: $ACTON_BASELINE"
    measure "$ACTON_BASELINE" baseline "$@"
fi
echo "work dir: $WORK"
