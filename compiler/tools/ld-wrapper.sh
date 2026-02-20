#!/usr/bin/env bash
set -euo pipefail

real_linker="${ACTON_REAL_LD:-gcc}"
state="static"
arch="$(uname -m 2>/dev/null || true)"
allow_dynamic_glibc=false
case "$arch" in
  aarch64|arm64|x86_64|amd64)
    allow_dynamic_glibc=true
    ;;
esac

args=()
args+=("-Wl,-Bstatic")
gcc_lib="$(gcc -print-file-name=libgcc_s.so 2>/dev/null || true)"
if [[ -n "$gcc_lib" && "$gcc_lib" != "libgcc_s.so" ]]; then
  args+=("-L$(dirname "$gcc_lib")")
fi

normalize_lib() {
  local lib="$1"
  if [[ "$lib" == :lib* ]]; then
    local name="${lib#:lib}"
    if [[ "$name" == *.so* ]]; then
      echo "${name%%.so*}"
      return
    fi
    if [[ "$name" == *.a ]]; then
      echo "${name%.a}"
      return
    fi
  fi
  echo "$lib"
}

handle_lib() {
  local lib="$1"
  local lib_key
  lib_key="$(normalize_lib "$lib")"
  if [[ "$lib_key" == "z" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    args+=("-l:libz.a")
    return
  fi
  if [[ "$lib_key" == "gmp" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    args+=("-l:libgmp.a")
    return
  fi
  if [[ "$lib_key" == "stdc++" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    args+=("-l:libstdc++.a")
    return
  fi
  if [[ "$allow_dynamic_glibc" == true ]]; then
    case "$lib_key" in
      c|m|dl|pthread|rt|util|gcc_s)
        if [[ "$state" != "dynamic" ]]; then
          args+=("-Wl,-Bdynamic")
          state="dynamic"
        fi
        args+=("-l${lib}")
        return
        ;;
    esac
  fi
  if [[ "$state" != "static" ]]; then
    args+=("-Wl,-Bstatic")
    state="static"
  fi
  args+=("-l${lib}")
}

process_arg() {
  local arg="$1"
  case "$arg" in
    @*)
      local rsp="${arg#@}"
      if [[ -f "$rsp" ]]; then
        while IFS= read -r line; do
          [[ -z "$line" ]] && continue
          if [[ "$line" == \"*\" && "$line" == *\" ]]; then
            line="${line:1:${#line}-2}"
            line="${line//\\\\/\\}"
            line="${line//\\\"/\"}"
          fi
          process_arg "$line"
        done < "$rsp"
      else
        args+=("$arg")
      fi
      ;;
    -Wl,*)
      IFS=',' read -r -a wl_opts <<< "${arg#-Wl,}"
      for opt in "${wl_opts[@]}"; do
        case "$opt" in
          -Bstatic|-Bdynamic)
            ;;
          -l*)
            handle_lib "${opt#-l}"
            ;;
          -u\ *)
            args+=("-Wl,-u" "-Wl,${opt#-u }")
            ;;
          *)
            if [[ "$opt" == *" "* ]]; then
              for part in $opt; do
                args+=("-Wl,$part")
              done
            else
              args+=("-Wl,$opt")
            fi
            ;;
        esac
      done
      ;;
    -l*)
      handle_lib "${arg#-l}"
      ;;
    -Wl,-Bstatic|-Wl,-Bdynamic)
      # Handled by this wrapper.
      ;;
    *)
      args+=("$arg")
      ;;
  esac
}

for arg in "$@"; do
  process_arg "$arg"
done

if [[ "$allow_dynamic_glibc" == true && "$state" != "dynamic" ]]; then
  args+=("-Wl,-Bdynamic")
  state="dynamic"
fi

exec "$real_linker" "${args[@]}"
