#!/usr/bin/env bash

set -euo pipefail

ACTON_BIN="${ACTON_BIN:-$(command -v acton)}"
OUT_DIR="${OUT_DIR:-${GITHUB_WORKSPACE:-$(pwd)}/debug-out}"
WORK_ROOT="${WORK_ROOT:-${RUNNER_TEMP:-/tmp}/acton-arm64-incremental-repro}"
PLAIN_ATTEMPTS="${PLAIN_ATTEMPTS:-200}"
GDB_ATTEMPTS="${GDB_ATTEMPTS:-25}"
ACTON_RTS_FLAGS="${ACTON_RTS_FLAGS:-}"
BUILD_TIMEOUT_SECONDS="${BUILD_TIMEOUT_SECONDS:-180}"
ACTON_REAL_BIN="$(readlink -f "${ACTON_BIN}" 2>/dev/null || printf '%s\n' "${ACTON_BIN}")"

ACTON_RTS_ARGS=()
if [ -n "${ACTON_RTS_FLAGS}" ]; then
    # The workflow supplies simple whitespace-separated RTS words.
    read -r -a ACTON_RTS_ARGS <<< "${ACTON_RTS_FLAGS}"
fi

mkdir -p "${OUT_DIR}" "${WORK_ROOT}"
SUMMARY="${OUT_DIR}/summary.txt"
: > "${SUMMARY}"

log() {
    printf '%s\n' "$*" | tee -a "${SUMMARY}"
}

run_acton() {
    local -a cmd=("${ACTON_BIN}")

    if [ "${#ACTON_RTS_ARGS[@]}" -gt 0 ]; then
        cmd+=("${ACTON_RTS_ARGS[@]}")
    fi
    cmd+=("$@")

    if [ "${BUILD_TIMEOUT_SECONDS}" != "0" ]; then
        timeout --preserve-status "${BUILD_TIMEOUT_SECONDS}" "${cmd[@]}"
    else
        "${cmd[@]}"
    fi
}

run_gdb_acton() {
    local -a cmd=("${ACTON_BIN}")

    if [ "${#ACTON_RTS_ARGS[@]}" -gt 0 ]; then
        cmd+=("${ACTON_RTS_ARGS[@]}")
    fi
    cmd+=("$@")

    if [ "${BUILD_TIMEOUT_SECONDS}" != "0" ]; then
        timeout --preserve-status "${BUILD_TIMEOUT_SECONDS}" gdb -q -batch \
            -ex "set pagination off" \
            -ex "set print thread-events off" \
            -ex "run" \
            -ex "thread apply all bt full" \
            -ex "info registers" \
            -ex 'x/24i $pc-32' \
            -ex 'disassemble $pc-64,$pc+128' \
            -ex "quit" \
            --args "${cmd[@]}"
    else
        gdb -q -batch \
            -ex "set pagination off" \
            -ex "set print thread-events off" \
            -ex "run" \
            -ex "thread apply all bt full" \
            -ex "info registers" \
            -ex 'x/24i $pc-32' \
            -ex 'disassemble $pc-64,$pc+128' \
            -ex "quit" \
            --args "${cmd[@]}"
    fi
}

write_build_act() {
    local dir="$1"
    local name="$2"
    local fingerprint="$3"
    mkdir -p "${dir}"
    {
        printf 'name = "%s"\n' "${name}"
        printf 'fingerprint = %s\n' "${fingerprint}"
        if [ "${name}" = "incremental_cases" ]; then
            printf '\n'
            printf 'dependencies = {\n'
            printf '  "libfoo": (path="deps/libfoo")\n'
            printf '}\n'
        fi
    } > "${dir}/Build.act"
}

prepare_project() {
    local proj="$1"
    local dep="${proj}/deps/libfoo"

    rm -rf "${proj}"
    mkdir -p "${proj}/src" "${dep}/src"

    write_build_act "${proj}" "incremental_cases" "0x0b4851fb00000001"
    write_build_act "${dep}" "libfoo" "0xb8cbf80500000001"

    cat > "${dep}/src/lib.act" <<'EOF_LIB_ORIG'
# Initial version
def calculate(x: int) -> int:
    return x * 2
EOF_LIB_ORIG

    cat > "${proj}/src/main.act" <<'EOF_MAIN'
import libfoo.lib

actor main(env: Env):
    result = libfoo.lib.calculate(21)
    print("Result: %d" % result)
    env.exit(0)
EOF_MAIN
}

modify_dep_impl() {
    local proj="$1"
    local factor="$2"
    cat > "${proj}/deps/libfoo/src/lib.act" <<'EOF_LIB_MOD'
# Modified version
def calculate(x: int) -> int:
EOF_LIB_MOD
    printf '    return x * %s\n' "${factor}" >> "${proj}/deps/libfoo/src/lib.act"
}

copy_failure_state() {
    local label="$1"
    local attempt_dir="$2"
    local target="${OUT_DIR}/${label}"

    rm -rf "${target}"
    mkdir -p "${target}"
    cp -a "${attempt_dir}" "${target}/attempt"
    cp -a "${OUT_DIR}"/*.log "${target}/" 2>/dev/null || true
}

collect_core_backtraces() {
    local label="$1"
    local cores_dir="${OUT_DIR}/${label}/cores"

    mkdir -p "${cores_dir}"
    find /tmp "${OUT_DIR}" -maxdepth 2 -type f -name 'core*' -print -exec cp -a {} "${cores_dir}/" \; 2>/dev/null || true

    if find "${cores_dir}" -type f -name 'core*' | grep -q .; then
        for core in "${cores_dir}"/core*; do
            [ -f "${core}" ] || continue
            file "${core}" > "${core}.file.txt" 2>&1 || true
            gdb -q -batch \
                -ex "set pagination off" \
                -ex "thread apply all bt full" \
                -ex "info registers" \
                -ex 'x/24i $pc-32' \
                -ex 'disassemble $pc-64,$pc+128' \
                -ex "info files" \
                "${ACTON_REAL_BIN}" "${core}" > "${core}.gdb.txt" 2>&1 || true
        done
    else
        log "No core files found for ${label}."
    fi
}

run_initial_build() {
    local proj="$1"
    local log_file="$2"

    (
        cd "${proj}"
        run_acton build --color never --jobs 1
    ) > "${log_file}" 2>&1
}

run_plain_rebuild() {
    local proj="$1"
    local log_file="$2"
    local status

    set +e
    (
        cd "${proj}"
        ulimit -c unlimited || true
        run_acton build --color never --verbose --jobs 1
    ) > "${log_file}" 2>&1
    status=$?
    set -e

    return "${status}"
}

run_gdb_rebuild() {
    local proj="$1"
    local log_file="$2"
    local status

    set +e
    (
        cd "${proj}"
        run_gdb_acton build --color never --verbose --jobs 1
    ) > "${log_file}" 2>&1
    status=$?
    set -e

    return "${status}"
}

run_plain_loop() {
    local attempts="$1"
    local attempt_dir="${WORK_ROOT}/plain"
    local proj="${attempt_dir}/incremental_cases"
    local initial_log="${OUT_DIR}/plain-initial.log"
    local status

    log "Starting plain repro loop with ${attempts} attempts."
    prepare_project "${proj}"
    log "plain loop: initial build"
    status=0
    run_initial_build "${proj}" "${initial_log}" || status=$?
    if [ "${status}" -ne 0 ]; then
        log "plain loop: initial build failed with status ${status}"
        copy_failure_state "plain-initial-failure" "${attempt_dir}"
        return "${status}"
    fi

    for attempt in $(seq 1 "${attempts}"); do
        local rebuild_log="${OUT_DIR}/plain-${attempt}-rebuild.log"
        local factor

        if [ $((attempt % 2)) -eq 1 ]; then
            factor=3
        else
            factor=2
        fi
        modify_dep_impl "${proj}" "${factor}"
        log "plain attempt ${attempt}: incremental rebuild after factor ${factor}"
        status=0
        run_plain_rebuild "${proj}" "${rebuild_log}" || status=$?
        if [ "${status}" -eq 0 ]; then
            if ! grep -q "impl changes in libfoo.lib.calculate" "${rebuild_log}"; then
                log "plain attempt ${attempt}: rebuild succeeded but expected impl-change trace was absent"
            fi
            rm -f "${rebuild_log}"
            continue
        fi

        log "plain attempt ${attempt}: rebuild failed with status ${status}"
        copy_failure_state "plain-failure-${attempt}" "${attempt_dir}"
        collect_core_backtraces "plain-failure-${attempt}"
        return "${status}"
    done

    rm -rf "${attempt_dir}" "${initial_log}"
    log "Plain repro loop completed without a failing rebuild."
}

run_gdb_loop() {
    local attempts="$1"
    local attempt_dir="${WORK_ROOT}/gdb"
    local proj="${attempt_dir}/incremental_cases"
    local initial_log="${OUT_DIR}/gdb-initial.log"
    local status

    log "Starting gdb repro loop with ${attempts} attempts."
    prepare_project "${proj}"
    log "gdb loop: initial build"
    status=0
    run_initial_build "${proj}" "${initial_log}" || status=$?
    if [ "${status}" -ne 0 ]; then
        log "gdb loop: initial build failed with status ${status}"
        copy_failure_state "gdb-initial-failure" "${attempt_dir}"
        return "${status}"
    fi

    for attempt in $(seq 1 "${attempts}"); do
        local rebuild_log="${OUT_DIR}/gdb-${attempt}-rebuild.gdb.log"
        local factor

        if [ $((attempt % 2)) -eq 1 ]; then
            factor=3
        else
            factor=2
        fi
        modify_dep_impl "${proj}" "${factor}"
        log "gdb attempt ${attempt}: incremental rebuild under gdb after factor ${factor}"
        status=0
        run_gdb_rebuild "${proj}" "${rebuild_log}" || status=$?
        if [ "${status}" -eq 0 ]; then
            if grep -q "Program received signal" "${rebuild_log}"; then
                log "gdb attempt ${attempt}: gdb captured a signal despite zero exit status"
                copy_failure_state "gdb-signal-${attempt}" "${attempt_dir}"
                return 1
            fi
            rm -f "${rebuild_log}"
            continue
        fi

        log "gdb attempt ${attempt}: rebuild failed with gdb status ${status}"
        copy_failure_state "gdb-failure-${attempt}" "${attempt_dir}"
        return "${status}"
    done

    rm -rf "${attempt_dir}" "${initial_log}"
    log "Gdb repro loop completed without a failing rebuild."
}

main() {
    log "Acton binary: ${ACTON_BIN}"
    log "Resolved Acton binary: ${ACTON_REAL_BIN}"
    log "Acton RTS flags: ${ACTON_RTS_FLAGS:-<default>}"
    log "Per-build timeout seconds: ${BUILD_TIMEOUT_SECONDS}"
    file "${ACTON_BIN}" "${ACTON_REAL_BIN}" | tee -a "${SUMMARY}" || true
    run_acton version | tee -a "${SUMMARY}" || true
    ldd "${ACTON_REAL_BIN}" > "${OUT_DIR}/acton.ldd.txt" 2>&1 || true
    dpkg -L acton > "${OUT_DIR}/acton.dpkg-files.txt" 2>&1 || true
    cp -a "${ACTON_REAL_BIN}" "${OUT_DIR}/acton-binary" 2>/dev/null || true
    objdump -h "${ACTON_REAL_BIN}" > "${OUT_DIR}/acton.objdump-sections.txt" 2>&1 || true

    plain_status=0
    run_plain_loop "${PLAIN_ATTEMPTS}" || plain_status=$?
    if [ "${plain_status}" -ne 0 ]; then
        log "Plain repro loop failed with status ${plain_status}; running gdb follow-up attempts before exiting."
        run_gdb_loop "${GDB_ATTEMPTS}" || true
        exit "${plain_status}"
    fi

    run_gdb_loop "${GDB_ATTEMPTS}"
}

main "$@"
