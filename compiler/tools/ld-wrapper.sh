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

add_static_gcc_lib() {
  local archive="$1"
  local fallback="$2"
  local archive_path
  if archive_path="$(gcc_lib_path "$archive")"; then
    args+=("$archive_path")
  else
    args+=("-l:$fallback")
  fi
}

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
    add_static_gcc_lib libz.a libz.a
    return
  fi
  if [[ "$lib_key" == "gmp" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    add_static_gcc_lib libgmp.a libgmp.a
    return
  fi
  if [[ "$lib_key" == "stdc++" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    add_static_gcc_lib libstdc++.a libstdc++.a
    return
  fi
  if [[ "$lib_key" == "tinfo" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    add_static_gcc_lib libtinfo.a libtinfo.a
    return
  fi
  if [[ "$allow_dynamic_glibc" == true ]]; then
    case "$lib_key" in
      c|m|dl|pthread|rt|util)
        if [[ "$state" != "dynamic" ]]; then
          args+=("-Wl,-Bdynamic")
          state="dynamic"
        fi
        args+=("-l${lib}")
        return
        ;;
    esac
  fi
  if [[ "$lib_key" == "gcc_s" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    add_static_gcc_lib libgcc_eh.a libgcc_eh.a
    return
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
