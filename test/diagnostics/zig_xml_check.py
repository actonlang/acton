"""Compare two exact CI packages on one host without changing test deadlines."""

import hashlib
import json
import os
from pathlib import Path
import signal
import subprocess
import time


ROOT = Path.cwd()
RESULTS = ROOT / "xml-results"
SOURCE = "2b21f7879130420cffe21ad2906f0e8ceb6182f0"
PACKAGES = {
    "baseline": {"run": 41627152742, "artifact": 13227945065,
                 "source": "b2d512559a719e35fd1f267c4edfb7cbacc934c7"},
    "candidate": {"run": 41694428931, "artifact": 13261192012,
                  "source": SOURCE},
}


def sha(data):
    return hashlib.sha256(data).hexdigest()


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2) + "\n")


def run(label, args, cwd, timeout):
    start = time.monotonic()
    timed_out = False
    with (RESULTS / (label + ".stdout")).open("wb") as out, \
            (RESULTS / (label + ".stderr")).open("wb") as err:
        proc = subprocess.Popen(args, cwd=cwd, stdout=out, stderr=err,
                                start_new_session=True)
        try:
            proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            timed_out = True
        finally:
            # Kill the group even if its leader exited with descendants alive.
            try:
                os.killpg(proc.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            proc.wait()
    result = {"label": label, "argv": args, "exit_code": proc.returncode,
              "outer_timeout": timed_out, "wall_seconds": time.monotonic() - start}
    write_json(RESULTS / (label + ".status.json"), result)
    print(json.dumps(result), flush=True)
    return result


def record_c(lane, dist, project):
    paths = [project / "out/types/stdlib_tests/test_xml.c",
             dist / "base/out/types/xml.c",
             dist / "std/out/types/std/xml.c",
             dist / "base/out/types/testing.c",
             dist / "base/src/xml.ext.c"]
    manifest = {}
    for path in paths:
        data = path.read_bytes()
        key = str(path.relative_to(project if path == paths[0] else dist))
        # Retain the original too. Only ignore path prefixes and the metadata
        # hash comment when comparing emitted C across compiler revisions.
        lines = data.splitlines(keepends=True)
        if lines and lines[0].startswith(b"/* Acton impl hash:"):
            lines = lines[1:]
        normalized = b"".join(lines).replace(str(project).encode(), b"<project>")
        normalized = normalized.replace(str(dist).encode(), b"<distribution>")
        manifest[key] = {"sha256": sha(data), "normalized_sha256": sha(normalized)}
        saved = RESULTS / lane / key
        saved.parent.mkdir(parents=True, exist_ok=True)
        saved.write_bytes(data)
    write_json(RESULTS / (lane + "-c-hashes.json"), manifest)
    return manifest


def read_report(row, isolated):
    report = json.loads((RESULTS / (row["label"] + ".stdout")).read_text())
    names = [test["name"] for test in report["tests"]]
    if isolated:
        assert names == ["xml_cdata_large"], names
    else:
        assert len(names) > 1 and "xml_cdata_large" in names, names
    assert all(not test["cached"] for test in report["tests"]), report
    row["report"] = report


def main():
    RESULTS.mkdir()
    (RESULTS / "cpuinfo.txt").write_bytes(Path("/proc/cpuinfo").read_bytes())
    run("host", ["sh", "-c", "uname -a; nproc; cat /proc/loadavg; cat /proc/meminfo; cat /etc/os-release"], ROOT, 10)
    source_root = ROOT / "input-source"
    assert subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=source_root, text=True).strip() == SOURCE
    source_files = {}
    for name in ["Build.act", "src/test_xml.act"]:
        data = (source_root / "test/stdlib_tests" / name).read_bytes()
        source_files[name] = data
    write_json(RESULTS / "source.json", {
        "revision": SOURCE,
        "files": {name: sha(data) for name, data in source_files.items()},
        "diagnostic_revision": subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip(),
    })
    prepared = {}
    for lane, metadata in PACKAGES.items():
        package_dir = ROOT / "packages" / lane
        archives = list(package_dir.glob("acton-linux-x86_64-*.tar.xz"))
        assert len(archives) == 1, archives
        archive = archives[0]
        subprocess.run(["tar", "-xJf", str(archive), "-C", str(package_dir)], check=True)
        dist = package_dir / "acton"
        acton = str(dist / "bin/acton")
        metadata.update({"archive": archive.name, "archive_sha256": sha(archive.read_bytes()),
                         "acton_sha256": sha((dist / "bin/acton").read_bytes()),
                         "zig_sha256": sha((dist / "zig/zig").read_bytes())})
        write_json(RESULTS / (lane + "-package.json"), metadata)
        library_sources = [dist / "base/src/xml.act", dist / "std/src/xml.act",
                           dist / "base/src/testing.act", dist / "base/src/xml.ext.c"]
        library_sources += [path for path in (dist / "deps/libxml2").rglob("*")
                            if path.is_file() and path.suffix in (".c", ".h", ".zig")]
        write_json(RESULTS / (lane + "-library-source-hashes.json"), {
            str(path.relative_to(dist)): sha(path.read_bytes()) for path in sorted(library_sources)
        })
        for name, args in [("acton-version", [acton, "version"]),
                           ("zig-version", [str(dist / "zig/zig"), "version"])]:
            assert run(lane + "-" + name, args, ROOT, 20)["exit_code"] == 0
        project = ROOT / "projects" / lane
        for name, data in source_files.items():
            path = project / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(data)
        build = run(lane + "-build", [acton, "build", "--syspath", str(dist),
                    "--test", "--ccmd", "src/test_xml.act"], project, 600)
        if build["exit_code"] != 0:
            raise RuntimeError(lane + " prebuild failed; see retained logs")
        record_c(lane, dist, project)
        prepared[lane] = (acton, dist, project)
    rows = []
    # All ten invocations are serial, alternating package order. Do not use
    # --iter, --max-time, perf mode, or an RTS thread override here: ordinary
    # defaults preserve the original three-second watchdog and workload.
    for sample in range(1, 6):
        for lane, (acton, dist, project) in prepared.items():
            row = run(lane + "-isolated-" + str(sample),
                      [acton, "test", "--syspath", str(dist), "--no-cache", "--json",
                       "--module", "test_xml", "--name", "xml_cdata_large"], project, 45)
            read_report(row, isolated=True)
            rows.append(row)
            write_json(RESULTS / "results.json", rows)
    # Preserve normal test-process concurrency to check whether contention
    # within the module changes the isolated result.
    for lane, (acton, dist, project) in prepared.items():
        row = run(lane + "-module", [acton, "test", "--syspath", str(dist),
                  "--no-cache", "--json", "--module", "test_xml"], project, 90)
        read_report(row, isolated=False)
        rows.append(row)
        write_json(RESULTS / "results.json", rows)


if __name__ == "__main__":
    main()
