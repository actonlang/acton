#!/usr/bin/env bash
set -euo pipefail

# NOTE: this script is intentionally near-identical to its sibling zig-cc.sh
# (the only meaningful difference is `zig c++` vs `zig cc`). Keep the lib/header
# rewriting helpers below in sync between the two when editing either.

ROOT="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
ZIG="${ROOT}/dist/zig/zig"

run_zig_with_lock() {
  if ! command -v flock >/dev/null 2>&1; then
    echo "flock is required for Linux Zig compiler wrapper serialization" >&2
    return 1
  fi

  local lock_root="${ZIG_GLOBAL_CACHE_DIR:-${TMPDIR:-/tmp}/acton-zig-global-cache}"
  mkdir -p "$lock_root"
  flock "$lock_root/acton-zig-cc.lock" "${ZIG}" "$@"
}

gcc_lib_path() {
  local lib="$1"
  local lib_path
  lib_path="$(gcc -print-file-name="$lib" 2>/dev/null || true)"
  if [[ -n "$lib_path" && "$lib_path" != "$lib" ]]; then
    echo "$lib_path"
    return 0
  fi
  return 1
}

# Static archives we build ourselves under bdeps/out take precedence over the
# host's apt-installed .a files, so the acton binary links against libs targeting
# our chosen libc version rather than the build machine's.
bdeps_lib_path() {
  local archive="$1"
  if [[ -n "${ACTON_BDEPS_DIR:-}" && -f "${ACTON_BDEPS_DIR}/lib/${archive}" ]]; then
    echo "${ACTON_BDEPS_DIR}/lib/${archive}"
    return 0
  fi
  return 1
}

add_bdeps_include_dirs() {
  if [[ -n "${ACTON_BDEPS_DIR:-}" && -d "${ACTON_BDEPS_DIR}/include" ]]; then
    args+=("-I" "${ACTON_BDEPS_DIR}/include")
  fi
}

add_system_include_dirs() {
  local multiarch
  args+=("-idirafter" "/usr/include")
  multiarch="$(gcc -print-multiarch 2>/dev/null || true)"
  if [[ -n "$multiarch" && -d "/usr/include/$multiarch" ]]; then
    args+=("-idirafter" "/usr/include/$multiarch")
  fi
}

add_arch_feature_args() {
  case "${ZIG_ARCH}" in
    x86_64)
      args+=("-Xclang" "-target-feature" "-Xclang" "+evex512")
      ;;
  esac
}

rewrite_lib_arg() {
  local arg="$1"
  local lib_path
  case "$arg" in
    -lgmp|-l:libgmp.a)
      if lib_path="$(bdeps_lib_path libgmp.a)"; then
        echo "$lib_path"
      elif lib_path="$(gcc_lib_path libgmp.a)"; then
        echo "$lib_path"
      else
        echo "$arg"
      fi
      ;;
    -lz|-l:libz.a)
      if lib_path="$(bdeps_lib_path libz.a)"; then
        echo "$lib_path"
      elif lib_path="$(gcc_lib_path libz.a)"; then
        echo "$lib_path"
      else
        echo "$arg"
      fi
      ;;
    -lstdc++|-l:libstdc++.a)
      # zig's bundled static libc++ replaces the host libstdc++.a.
      echo "-lc++"
      ;;
    -lgcc_s|-l:libgcc_s.so*|-lgcc_eh|-l:libgcc_eh.a)
      # zig's bundled libunwind provides the _Unwind_* symbols.
      echo "-lunwind"
      ;;
    -ltinfo|-l:libtinfo.a)
      if lib_path="$(bdeps_lib_path libtinfo.a)"; then
        echo "$lib_path"
      elif lib_path="$(gcc_lib_path libtinfo.a)"; then
        echo "$lib_path"
      else
        echo "$arg"
      fi
      ;;
    *)
      echo "$arg"
      ;;
  esac
}

process_arg() {
  local arg="$1"
  case "$arg" in
    @*)
      local rsp="${arg#@}"
      if [[ -f "$rsp" ]]; then
        local line
        while IFS= read -r line || [[ -n "$line" ]]; do
          [[ -z "$line" ]] && continue
          if [[ "$line" == \"*\" && "$line" == *\" ]]; then
            line="${line:1:${#line}-2}"
            line="${line//\\\\/\\}"
            line="${line//\\\"/\"}"
          fi
          process_arg "$line"
        done < "$rsp"
      else
        args+=("$(rewrite_lib_arg "$arg")")
      fi
      ;;
    *)
      args+=("$(rewrite_lib_arg "$arg")")
      ;;
  esac
}

case "$(uname -s)" in
  Linux)
    ARCH="$(uname -m)"
    case "${ARCH}" in
      x86_64) ZIG_ARCH=x86_64 ;;
      aarch64|arm64) ZIG_ARCH=aarch64 ;;
      *) echo "Unsupported architecture for Linux: ${ARCH}" >&2; exit 1 ;;
    esac
    GLIBC_VERSION="${ACTON_ZIG_GLIBC_VERSION:-2.28}"
    args=()
    add_bdeps_include_dirs
    add_system_include_dirs
    add_arch_feature_args
    for arg in "$@"; do
      process_arg "$arg"
    done
    run_zig_with_lock c++ -target "${ZIG_ARCH}-linux-gnu.${GLIBC_VERSION}" "${args[@]}"
    ;;
  *)
    exec "${ZIG}" c++ "$@"
    ;;
esac
