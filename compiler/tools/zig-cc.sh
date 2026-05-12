#!/usr/bin/env bash
set -euo pipefail

ROOT="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
ZIG="${ROOT}/dist/zig/zig"

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

add_system_include_dirs() {
  local multiarch
  args+=("-idirafter" "/usr/include")
  multiarch="$(gcc -print-multiarch 2>/dev/null || true)"
  if [[ -n "$multiarch" && -d "/usr/include/$multiarch" ]]; then
    args+=("-idirafter" "/usr/include/$multiarch")
  fi
}

rewrite_lib_arg() {
  local arg="$1"
  local lib_path
  case "$arg" in
    -lgmp|-l:libgmp.a)
      if lib_path="$(gcc_lib_path libgmp.a)"; then
        echo "$lib_path"
      else
        echo "$arg"
      fi
      ;;
    -lz|-l:libz.a)
      if lib_path="$(gcc_lib_path libz.a)"; then
        echo "$lib_path"
      else
        echo "$arg"
      fi
      ;;
    -lstdc++|-l:libstdc++.a)
      if lib_path="$(gcc_lib_path libstdc++.a)"; then
        echo "$lib_path"
      else
        echo "$arg"
      fi
      ;;
    -ltinfo|-l:libtinfo.a)
      if lib_path="$(gcc_lib_path libtinfo.a)"; then
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
    GLIBC_VERSION="${ACTON_ZIG_GLIBC_VERSION:-2.27}"
    args=()
    add_system_include_dirs
    for arg in "$@"; do
      process_arg "$arg"
    done
    exec "${ZIG}" cc -target "${ZIG_ARCH}-linux-gnu.${GLIBC_VERSION}" "${args[@]}"
    ;;
  *)
    exec "${ZIG}" cc "$@"
    ;;
esac
