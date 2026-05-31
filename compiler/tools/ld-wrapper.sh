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

# Static archives we build ourselves under bdeps/out (libz, libgmp, libtinfo,
# liblmdb, ...). Linked by full path so the binary targets our chosen libc
# version rather than the host's apt-installed .a files.
bdeps_lib_path() {
  local archive="$1"
  if [[ -n "${ACTON_BDEPS_DIR:-}" && -f "${ACTON_BDEPS_DIR}/lib/${archive}" ]]; then
    echo "${ACTON_BDEPS_DIR}/lib/${archive}"
    return 0
  fi
  return 1
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
  # The C++ runtime and gcc's unwinder are replaced with zig's bundled libc++ and
  # libunwind (not taken from bdeps/out or the host) so they target our chosen
  # libc version. libc++ needs libunwind for the _Unwind_* symbols; -lunwind is
  # listed explicitly so it is available regardless of archive ordering.
  if [[ "$lib_key" == "stdc++" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    args+=("-lc++" "-lunwind")
    return
  fi
  if [[ "$lib_key" == "gcc_s" || "$lib_key" == "gcc_eh" ]]; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    args+=("-lunwind")
    return
  fi
  # glibc libraries stay dynamic -- we target a chosen glibc version, not a fully
  # static libc.
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
  # Everything we build under bdeps/out (libz, libgmp, libtinfo, liblmdb, ...) is
  # linked statically by its full path. A new compiler lib needs no case here --
  # just its .a present in bdeps/out/lib -- mirroring the macOS block above.
  local bdeps_archive
  if bdeps_archive="$(bdeps_lib_path "lib${lib_key}.a")"; then
    if [[ "$state" != "static" ]]; then
      args+=("-Wl,-Bstatic")
      state="static"
    fi
    args+=("$bdeps_archive")
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
