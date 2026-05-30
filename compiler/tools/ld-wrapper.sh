#!/usr/bin/env bash
set -euo pipefail

real_linker="${ACTON_REAL_LD:-gcc}"

# -- macOS (ld64) -------------------------------------------------------------
# ld64 has no -Bstatic/-Bdynamic and prefers libfoo.dylib over libfoo.a for a
# given -lfoo, so the GNU-ld toggling strategy below does not apply. Instead we
# rewrite each -lfoo whose libfoo.a we built under bdeps/out/lib to that
# archive's full path -- a positional arg ld64 links statically -- while every
# other argument passes through untouched: system -l flags keep resolving to
# dylibs, and GHC's ld64-specific flags (-no_fixup_chains, -U <sym>, ...) reach
# the real linker intact. That real linker is the system clang/ld64 GHC is
# configured for (ACTON_REAL_LD is left unset on macOS); zig's bundled linker
# rejects those ld64 flags, so it builds our archives but cannot do this link.
# Any future bdeps lib links statically just by having its .a present here -- no
# per-library list to maintain.
if [[ "$(uname -s)" == "Darwin" ]]; then
  macos_args=()
  macos_process_arg() {
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
            macos_process_arg "$line"
          done < "$rsp"
        else
          macos_args+=("$arg")
        fi
        ;;
      -l*)
        local name="${arg#-l}"
        # Normalise -l:libfoo.a / -l:libfoo.dylib to the bare library name.
        if [[ "$name" == :* ]]; then
          name="${name#:}"; name="${name#lib}"
          name="${name%.a}"; name="${name%.dylib}"
        fi
        if [[ -n "${ACTON_BDEPS_DIR:-}" && -f "${ACTON_BDEPS_DIR}/lib/lib${name}.a" ]]; then
          macos_args+=("${ACTON_BDEPS_DIR}/lib/lib${name}.a")
        else
          macos_args+=("$arg")
        fi
        ;;
      *)
        macos_args+=("$arg")
        ;;
    esac
  }
  for arg in "$@"; do
    macos_process_arg "$arg"
  done
  exec "$real_linker" "${macos_args[@]}"
fi

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

# Prefer static archives we build ourselves under bdeps/out over the host's
# apt-installed .a files, so the link targets our chosen libc version.
bdeps_lib_path() {
  local archive="$1"
  if [[ -n "${ACTON_BDEPS_DIR:-}" && -f "${ACTON_BDEPS_DIR}/lib/${archive}" ]]; then
    echo "${ACTON_BDEPS_DIR}/lib/${archive}"
    return 0
  fi
  return 1
}

add_static_lib() {
  local archive="$1"
  local fallback="$2"
  local archive_path
  if archive_path="$(bdeps_lib_path "$archive")"; then
    args+=("$archive_path")
  elif archive_path="$(gcc_lib_path "$archive")"; then
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
    add_static_lib libz.a libz.a
    return
  fi
  if [[ "$lib_key" == "gmp" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    add_static_lib libgmp.a libgmp.a
    return
  fi
  if [[ "$lib_key" == "stdc++" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    # Use zig's bundled static libc++ instead of the host's libstdc++.a so the
    # C++ runtime targets our chosen libc version. libc++ pulls in zig's
    # libunwind for the _Unwind_* symbols; -lunwind is listed explicitly so it
    # is available regardless of archive ordering.
    args+=("-lc++" "-lunwind")
    return
  fi
  if [[ "$lib_key" == "tinfo" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    add_static_lib libtinfo.a libtinfo.a
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
  if [[ "$lib_key" == "gcc_s" || "$lib_key" == "gcc_eh" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    args+=("-lunwind")
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
