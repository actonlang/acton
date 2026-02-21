#!/usr/bin/env sh
set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
ROOT_DIR="$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)"
ZIG_BIN="$ROOT_DIR/dist/zig/zig"

if [ ! -x "$ZIG_BIN" ]; then
  echo "error: expected Zig at $ZIG_BIN; run 'make dist/zig' first" >&2
  exit 1
fi

has_explicit_target=false
expect_target_arg=false
for arg in "$@"; do
  if [ "$expect_target_arg" = true ]; then
    has_explicit_target=true
    expect_target_arg=false
    continue
  fi

  case "$arg" in
    -target)
      expect_target_arg=true
      ;;
    -target=*)
      has_explicit_target=true
      ;;
    --target)
      expect_target_arg=true
      ;;
    --target=*)
      has_explicit_target=true
      ;;
  esac
done

if [ "$(uname -s 2>/dev/null || true)" = "Linux" ] \
   && [ -n "${ACTON_ZIG_GLIBC_VERSION:-}" ] \
   && [ "$has_explicit_target" = false ]; then
  zig_host_target="$("$ZIG_BIN" env 2>/dev/null | awk -F'"' '/\.target = "/ {print $2; exit}')"
  if [ -z "$zig_host_target" ]; then
    echo "error: failed to derive Zig host target from '$ZIG_BIN env'" >&2
    exit 1
  fi

  case "$zig_host_target" in
    *-gnu*)
      ;;
    *)
      echo "error: requested ACTON_ZIG_GLIBC_VERSION but host target '$zig_host_target' is not GNU libc based" >&2
      exit 1
      ;;
  esac

  zig_host_arch="${zig_host_target%%-*}"
  if [ -z "$zig_host_arch" ]; then
    echo "error: failed to derive host architecture from Zig target '$zig_host_target'" >&2
    exit 1
  fi

  exec "$ZIG_BIN" cc -target "$zig_host_arch-linux-gnu.${ACTON_ZIG_GLIBC_VERSION}" "$@"
fi

exec "$ZIG_BIN" cc "$@"
